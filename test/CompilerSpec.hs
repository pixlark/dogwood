{-# LANGUAGE QuasiQuotes #-}

module CompilerSpec (spec) where

import DW.Common
import DW.Compiler qualified as Compiler
import DW.Error (displayError)
import DW.IR
import DW.Logging (noOpLogger, runLog)
import DW.LoopPass qualified as LoopPass
import DW.LowerPass qualified as LowerPass
import DW.Parser qualified as Parser
import DW.Typechecker qualified as Typechecker
import DW.TypedAST (ValueTypeExpr (..), makeValueExpr)
import DW.Util (stripCallStacks)
import Data.Bifunctor (Bifunctor (first))
import Data.Text (unpack)
import NeatInterpolation
import Test.Hspec

testCompile :: Text -> IO (Either String Program)
testCompile source = do
  result <- runEff $ runLog noOpLogger $ runErrors $ do
    -- Passes 1 and 2: Lexing and parsing
    ast <- runErrorAsErrors $ Parser.runParser source Parser.parseTopLevel
    -- Pass 3: Lowering
    let loweredAST = LowerPass.runLowerPass ast
    -- Pass 4: Typechecking
    typedAST <- runErrorAsErrors $ Typechecker.runTypechecker loweredAST
    -- Pass 5: Loop validation
    LoopPass.runLoopPass typedAST
    -- Pass 6: Compile to IR
    Compiler.runCompiler typedAST
  return $ first (concatMap $ unpack . displayError source) (stripCallStacks result)

getPhis :: Program -> [Phi]
getPhis (Program fns) = concatMap (\(_, block) -> block.phis) (concatMap (\(FnDef _ blocks) -> blocks) fns)

-- | Check if a phi has all operands pointing to the same name (trivial)
isTrivialPhi :: Phi -> Bool
isTrivialPhi phi =
  let operandNames = filter (/= phi.name) $ snd <$> phi.operands
   in case operandNames of
        [] -> True -- unreachable
        (x : xs) -> all (== x) xs

spec :: Spec
spec = do
  describe "the Compiler module" do
    describe "performs phi reduction correctly" do
      it "reduces trivial phis when a variable is not modified in branches" do
        -- x is defined before the if-else and not modified in either branch,
        -- so reading x after the merge should NOT produce a phi (it gets reduced)
        let source =
              [text|
                let main = fn() {
                  let x: int = 5;
                  if true {
                    let y: int = 1;
                  } else {
                    let z: int = 2;
                  }
                  x
                };
              |]
        result <- testCompile source
        case result of
          Left err -> expectationFailure err
          Right program -> do
            let phis = getPhis program
            -- The only phi left should be the one determining the value of the if expression
            length phis `shouldBe` 1
            length [p | p <- phis, isTrivialPhi p] `shouldBe` 0
            length (filter (\p -> p.ty == makeValueExpr Void) phis) `shouldBe` 1

      it "does not reduce non-trivial phis when branches assign different values" do
        let source =
              [text|
                let main = fn() {
                  let x: int = 0;
                  if true {
                    x = 1;
                  } else {
                    x = 2;
                  }
                  x
                };
              |]
        result <- testCompile source
        case result of
          Left err -> expectationFailure err
          Right program -> do
            let phis = getPhis program
            -- There should be two phis now, one for the if expression, and one for the final value of x
            length phis `shouldBe` 2
            length [p | p <- phis, isTrivialPhi p] `shouldBe` 0
            length (filter (\p -> p.ty == makeValueExpr Void) phis) `shouldBe` 1
            length (filter (\p -> p.ty == makeValueExpr Int) phis) `shouldBe` 1

      it "reduces trivial phis in nested control flow" do
        let source =
              [text|
                let main = fn() {
                  let x: int = 5;
                  if true {
                    if false {
                      let a: int = 1;
                    } else {
                      let b: int = 2;
                    }
                  } else {
                    let c: int = 3;
                  }
                  x
                };
              |]
        result <- testCompile source
        case result of
          Left err -> expectationFailure err
          Right program -> do
            let phis = getPhis program
            -- There should be two phis, one for each if expression, but none for x
            length phis `shouldBe` 2
            length [p | p <- phis, isTrivialPhi p] `shouldBe` 0
            length (filter (\p -> p.ty == makeValueExpr Void) phis) `shouldBe` 2

      it "handles the case where one branch modifies and one doesn't" do
        let source =
              [text|
                let main = fn() {
                  let x: int = 5;
                  if true {
                    x = 10;
                  } else {
                    let y: int = 1;
                  }
                  x
                };
              |]
        result <- testCompile source
        case result of
          Left err -> expectationFailure err
          Right program -> do
            let phis = getPhis program
            length phis `shouldBe` 2
            length [p | p <- phis, isTrivialPhi p] `shouldBe` 0
            length (filter (\p -> p.ty == makeValueExpr Void) phis) `shouldBe` 1
            length (filter (\p -> p.ty == makeValueExpr Int) phis) `shouldBe` 1

      it "reduces chained trivial phis" do
        -- x flows through multiple merge points unchanged
        let source =
              [text|
                let main = fn() {
                  let x: int = 5;
                  if true {
                    let a: int = 1;
                  } else {
                    let b: int = 2;
                  }
                  if false {
                    let c: int = 3;
                  } else {
                    let d: int = 4;
                  }
                  x
                };
              |]
        result <- testCompile source
        case result of
          Left err -> expectationFailure err
          Right program -> do
            let phis = getPhis program
            length phis `shouldBe` 2
            length [p | p <- phis, isTrivialPhi p] `shouldBe` 0
            length (filter (\p -> p.ty == makeValueExpr Void) phis) `shouldBe` 2
