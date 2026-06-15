{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import AST
import qualified AST as A
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Error
import Lexer.Internal
import Parser.Internal
import Test.Hspec

runParse :: Text -> ParserM a -> Result a
runParse source f = case makeParser lexer of
  Left e -> Left e
  Right parser -> run parser
  where
    lexer = makeLexer source
    run = evalState $ runExceptT f

consumeSymbol = do
  current <- gets Parser.Internal.current
  case current of
    Symbol sym -> do Parser.Internal.advance; return $ Just sym
    _ -> return Nothing

parseCommas trailing = parseSeparatedSequence SeparatorConfig {trailing, separator = Glyph ",", consume = consumeSymbol}

main :: IO ()
main = hspec $ do
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
