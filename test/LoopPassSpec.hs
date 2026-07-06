{-# LANGUAGE QuasiQuotes #-}

module LoopPassSpec (spec) where

import DW.Common
import DW.Error
import DW.Logging (noOpLogger, runLog)
import DW.LoopPass qualified as LoopPass
import DW.LowerPass qualified as LowerPass
import DW.Parser qualified as Parser
import DW.Typechecker qualified as Typechecker
import DW.Util (stripCallStacks)
import Test.Hspec
import Prelude hiding (show)

testLoopPass source = fmap stripCallStacks $ runEff $ runErrors $ runLog noOpLogger do
  -- Passes 1 and 2: Lexing and parsing
  ast <- Parser.runParser source Parser.parseTopLevel
  -- Pass 3: Lowering
  let loweredAST = LowerPass.runLowerPass ast
  -- Pass 4: Typechecking
  typedAST <- Typechecker.runTypechecker loweredAST
  -- Pass 5: Loop validation
  LoopPass.runLoopPass typedAST

spec = do
  describe "the LoopPass module" do
    it "handles breaks correctly" do
      result <- testLoopPass "loop { break; }"
      result `shouldSatisfy` isRight

      result <- testLoopPass "break;"
      result `shouldSatisfy` isErrorKind BreakOutsideLoop
