{-# LANGUAGE QuasiQuotes #-}

module LoopPassSpec (spec) where

import Common
import Error
import LoopPass (runLoopPass)
import qualified LowerPass
import qualified Parser
import Test.Hspec
import qualified Typechecker
import Util (stripCallStack)
import Prelude hiding (show)

testLoopPass source = stripCallStack $ runPureEff $ runError $ do
  -- Passes 1 and 2: Lexing and parsing
  ast <- Parser.runParser source Parser.parseStmt
  -- Pass 3: Lowering
  let loweredAST = LowerPass.runLowerPass ast
  -- Pass 4: Typechecking
  typedAST <- Typechecker.runTypechecker loweredAST
  -- Pass 5: Loop validation
  LoopPass.runLoopPass typedAST

spec = do
  describe "the LoopPass module" do
    it "handles breaks correctly" do
      testLoopPass "loop { break; }" `shouldSatisfy` isRight
      testLoopPass "break;" `shouldSatisfy` isErrorKind BreakOutsideLoop
