{-# LANGUAGE QuasiQuotes #-}

module Main where

import CompilerSpec qualified
import EmitCSpec qualified
import ErrorsSpec qualified
import IntegrationSpec qualified
import LexerSpec qualified
import LoopPassSpec qualified
import LowerPassSpec qualified
import NameResolutionSpec qualified
import ParserSpec qualified
import Test.Hspec
import TypecheckerSpec qualified
import UnifyTestSpec qualified

main :: IO ()
main = hspec $ do
  ErrorsSpec.spec
  LexerSpec.spec
  ParserSpec.spec
  LowerPassSpec.spec
  NameResolutionSpec.spec
  TypecheckerSpec.spec
  LoopPassSpec.spec
  CompilerSpec.spec
  EmitCSpec.spec
  IntegrationSpec.spec
  UnifyTestSpec.spec
