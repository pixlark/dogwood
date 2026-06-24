{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LoopPassSpec (spec) where

import AST (AST)
import qualified AST as A
import Data.Either (isRight)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, show)
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.State.Static.Local
import Error
import LoopPass (runLoopPass)
import NeatInterpolation
import Parser (parseStmt, runParse)
import Test.Hspec
import Typechecker.Internal
import TypedAST (TST)
import qualified TypedAST as T
import Prelude hiding (show)

spec = do
  describe "the LoopPass module" do
    it "handles breaks correctly" do
      runLoopPass "loop { break; }" `shouldSatisfy` isRight
      runLoopPass "break;" `shouldSatisfy` isErrorKind BreakOutsideLoop
