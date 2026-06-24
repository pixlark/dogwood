{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
