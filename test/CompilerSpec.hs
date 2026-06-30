{-# LANGUAGE QuasiQuotes #-}

module CompilerSpec (spec) where

import Common
import Compiler
import Data.Text (append, show)
import Effectful (runEff)
import IR
import LexicalScopes (mkScopes)
import Logging (noOpLogger, runLog)
import NeatInterpolation
import Test.Hspec
import TypedAST
import Prelude hiding (show)

spec =
  describe "the Compiler module" do
    return undefined
