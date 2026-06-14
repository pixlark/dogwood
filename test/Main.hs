{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AST as A
import Control.Monad.State.Lazy
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Lexer.Internal
import Parser.Internal
import Test.Hspec

type ParserResult a = Parser.Internal.Result a

runParse :: Text -> ParserM (ParserResult a) -> ParserResult a
runParse source f = case makeParser lexer of
  Left e -> Left e
  Right parser -> run parser
  where
    lexer = makeLexer source
    run = evalState f

main :: IO ()
main = hspec $ do
  describe "the Parser module" $ do
    it "can parse builtin types" $ do
      runParse "void" parseBuiltinType `shouldBe` Right A.Void
      runParse "bool" parseBuiltinType `shouldBe` Right A.Bool
      runParse "int" parseBuiltinType `shouldBe` Right A.Int
      runParse "asdf" parseBuiltinType `shouldSatisfy` isLeft
