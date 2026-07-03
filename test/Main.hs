{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified CompilerSpec
import qualified EmitCSpec
import qualified IntegrationSpec
import qualified LexerSpec
import qualified LoopPassSpec
import qualified LowerPassSpec
import qualified ParserSpec
import Test.Hspec
import qualified TypecheckerSpec

main :: IO ()
main = hspec $ do
  LexerSpec.spec
  ParserSpec.spec
  LowerPassSpec.spec
  TypecheckerSpec.spec
  LoopPassSpec.spec
  CompilerSpec.spec
  EmitCSpec.spec
  IntegrationSpec.spec
