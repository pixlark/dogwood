{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified LexerSpec
import qualified LoopPassSpec
import qualified ParserSpec
import Test.Hspec
import qualified TypecheckerSpec

main :: IO ()
main = hspec $ do
  LexerSpec.spec
  ParserSpec.spec
  TypecheckerSpec.spec
  LoopPassSpec.spec
