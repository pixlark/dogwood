{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AST as A
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Lexer.Internal
import Parser.Internal
import Test.Hspec

type ParserResult a = Parser.Internal.Result a

runParse :: Text -> ParserM a -> ParserResult a
runParse source f = case makeParser lexer of
  Left e -> Left e
  Right parser -> run parser
  where
    lexer = makeLexer source
    run = evalState $ runExceptT f

-- runParseN :: Text -> ParserN a -> Maybe a
-- runParseN source f = case makeParser lexer of
--   Left e -> Nothing
--   Right parser -> run parser
--   where
--     lexer = makeLexer source
--     run = evalState f

consumeSymbol = do
  current <- gets Parser.Internal.current
  case current of
    Symbol sym -> do Parser.Internal.advance; return $ Just sym
    _ -> return Nothing

parseCommas trailing = parseSeparatedSequence (SeparatorConfig {trailing, separator = Glyph ",", consume = consumeSymbol})

main :: IO ()
main = hspec $ do
  describe "the Parser module" $ do
    it "can parse builtin types" $ do
      runParse "void" parseBuiltinType `shouldBe` Right A.Void
      runParse "bool" parseBuiltinType `shouldBe` Right A.Bool
      runParse "int" parseBuiltinType `shouldBe` Right A.Int
      runParse "asdf" parseBuiltinType `shouldSatisfy` isLeft
    -- it "fooN" $ do
    --   runParseN "&&" parseFooN `shouldBe` Just "foo"
    --   runParseN "&" parseFooN `shouldBe` Nothing
    -- it "foo" $ do
    --   runParse "&&" parseFoo `shouldBe` Right "foo"
    --   runParse "&" parseFoo `shouldSatisfy` isLeft
    it "can parse separated sequences" $ do
      runParse "a, b, c" (parseCommas False) `shouldBe` Right ["a", "b", "c"]
      runParse "a, b, c," (parseCommas False) `shouldSatisfy` isLeft
      runParse "a, b, c," (parseCommas True) `shouldBe` Right ["a", "b", "c"]
      runParse "a,, b, c" (parseCommas False) `shouldSatisfy` isLeft
      runParse "a" (parseCommas True) `shouldBe` Right ["a"]
      runParse "" (parseCommas True) `shouldBe` Right []
