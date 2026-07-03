{-# LANGUAGE QuasiQuotes #-}

module LoopPassSpec (spec) where

import DW.Common
import DW.Error
import DW.Logging (noOpLogger, runLog)
import qualified DW.LoopPass as LoopPass
import qualified DW.LowerPass as LowerPass
import qualified DW.Parser as Parser
import Test.Hspec
import qualified DW.Typechecker as Typechecker
import DW.Util (stripCallStack)
import Prelude hiding (show)

testLoopPass source = fmap stripCallStack $ runEff $ runError $ runLog noOpLogger do
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
      result <- testLoopPass "loop { break; }"
      result `shouldSatisfy` isRight

      result <- testLoopPass "break;"
      result `shouldSatisfy` isErrorKind BreakOutsideLoop
