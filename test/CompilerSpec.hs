{-# LANGUAGE QuasiQuotes #-}

module CompilerSpec (spec) where

import Common
import Compiler
import Data.List (nub)
import Data.Text (unpack)
import Error (displayError)
import IR
import Logging (noOpLogger, runLog)
import NeatInterpolation
import Test.Hspec
import TypedAST (ValueTypeExpr (..), makeValueExpr)

-- | Helper to run the compiler and return the resulting program
testCompile :: Text -> IO (Either Text Program)
testCompile source = do
  result <- runEff $ runLog noOpLogger $ runCompiler source
  return $ case result of
    Left err -> Left (displayError source err)
    Right program -> Right program

-- | Get all phi instructions from a program
getPhis :: Program -> [Phi]
getPhis (Program blocks) = concatMap (\(_, block) -> block.phis) blocks

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
                {
                  let x: int = 5;
                  if true {
                    let y: int = 1;
                  } else {
                    let z: int = 2;
                  }
                  x
                }
              |]
        result <- testCompile source
        case result of
          Left err -> expectationFailure (unpack err)
          Right program -> do
            let phis = getPhis program
            -- The only phi left should be the one determining the value of the if expression
            length phis `shouldBe` 1
            length [p | p <- phis, isTrivialPhi p] `shouldBe` 0
            length (filter (\p -> p.ty == makeValueExpr Void) phis) `shouldBe` 1

      it "does not reduce non-trivial phis when branches assign different values" do
        let source =
              [text|
                {
                  let x: int = 0;
                  if true {
                    x = 1;
                  } else {
                    x = 2;
                  }
                  x
                }
              |]
        result <- testCompile source
        case result of
          Left err -> expectationFailure (unpack err)
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
                {
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
                }
              |]
        result <- testCompile source
        case result of
          Left err -> expectationFailure (unpack err)
          Right program -> do
            let phis = getPhis program
            -- There should be two phis, one for each if expression, but none for x
            length phis `shouldBe` 2
            length [p | p <- phis, isTrivialPhi p] `shouldBe` 0
            length (filter (\p -> p.ty == makeValueExpr Void) phis) `shouldBe` 2

      it "handles the case where one branch modifies and one doesn't" do
        let source =
              [text|
                {
                  let x: int = 5;
                  if true {
                    x = 10;
                  } else {
                    let y: int = 1;
                  }
                  x
                }
              |]
        result <- testCompile source
        case result of
          Left err -> expectationFailure (unpack err)
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
                {
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
                }
              |]
        result <- testCompile source
        case result of
          Left err -> expectationFailure (unpack err)
          Right program -> do
            let phis = getPhis program
            length phis `shouldBe` 2
            length [p | p <- phis, isTrivialPhi p] `shouldBe` 0
            length (filter (\p -> p.ty == makeValueExpr Void) phis) `shouldBe` 2
