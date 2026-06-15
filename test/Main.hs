{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import AST
import qualified AST as A
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State.Lazy
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Error
import Lexer.Internal
import Parser.Internal
import Test.Hspec

runLex :: Text -> Result [Token]
runLex source = run lexer
  where
    lexer = makeLexer source
    maybeNextToken = do
      tok <- ExceptT nextToken
      return $ if tok.kind == Eof then Nothing else Just tok
    run = evalState $ runExceptT $ unfoldM maybeNextToken

runParse :: Text -> ParserM a -> Result a
runParse source f = case makeParser lexer of
  Left e -> Left e
  Right parser -> run parser
  where
    lexer = makeLexer source
    run = evalState $ runExceptT f

consumeSymbol = do
  current <- gets Parser.Internal.current
  case current.kind of
    Symbol sym -> do Parser.Internal.advance; return $ Just sym
    _ -> return Nothing

parseCommas trailing = parseSeparatedSequence SeparatorConfig {trailing, separator = Glyph ",", consume = consumeSymbol}

main :: IO ()
main = hspec $ do
  describe "the Lexer module" $ do
    it "tracks source spans correctly" $ do
      runLex "asdf, void"
        `shouldBe` Right
          [ Token (Symbol "asdf") (Span 0 4),
            Token (Glyph ",") (Span 4 1),
            Token (Keyword "void") (Span 6 4)
          ]
  describe "the Parser module" $ do
    it "can parse separated sequences" $ do
      runParse "a, b, c" (parseCommas False) `shouldBe` Right ["a", "b", "c"]
      runParse "a, b, c," (parseCommas False) `shouldBe` Left ExpectedAnotherElementOfSequence
      runParse "a, b, c," (parseCommas True) `shouldBe` Right ["a", "b", "c"]
      runParse "a,, b, c" (parseCommas False) `shouldBe` Left ExpectedAnotherElementOfSequence
      runParse "a" (parseCommas True) `shouldBe` Right ["a"]
      runParse "" (parseCommas True) `shouldBe` Right []
    it "can parse namespaced identifiers" $ do
      runParse "foo::bar" parseNamespacedIdentifier `shouldBe` Right (NamespacedIdentifier ["foo", "bar"])
      runParse "foo::bar::" parseNamespacedIdentifier `shouldBe` Left ExpectedAnotherElementOfSequence
      runParse "foo" parseNamespacedIdentifier `shouldBe` Right (NamespacedIdentifier ["foo"])
    it "can parse type expressions" $ do
      runParse "void" parseTypeExpr `shouldBe` Right (makeValueExpr A.Void)
      runParse "bool" parseTypeExpr `shouldBe` Right (makeValueExpr A.Bool)
      runParse "int" parseTypeExpr `shouldBe` Right (makeValueExpr A.Int)
      runParse "asdf" parseTypeExpr `shouldBe` Right (makeValueExpr $ NamespacedIdentifier ["asdf"])
      runParse "asdf::foo" parseTypeExpr `shouldBe` Right (makeValueExpr $ NamespacedIdentifier ["asdf", "foo"])
      runParse "&bool" parseTypeExpr `shouldBe` Right (makeReferenceExpr A.Bool)
      runParse "&&bool" parseTypeExpr `shouldBe` Left ExpectedTypeExpr
