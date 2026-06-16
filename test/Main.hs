{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import AST (AST (..))
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
      tok <- nextToken
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

buildAST :: Int -> Int -> a -> AST a
buildAST start length inner = AST inner (Span start length)

expectAST :: Int -> Int -> a -> Result (AST a)
expectAST start length inner = Right $ buildAST start length inner

main :: IO ()
main = hspec $ do
  describe "the Lexer module" $ do
    it "tracks source spans correctly" $ do
      runLex "asdf, void "
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
    it "can parse type expressions" $ do
      runParse "void" parseTypeExpr `shouldBe` expectAST 0 4 (A.makeValueExpr A.Void)
      runParse "bool" parseTypeExpr `shouldBe` expectAST 0 4 (A.makeValueExpr A.Bool)
      runParse "int" parseTypeExpr `shouldBe` expectAST 0 3 (A.makeValueExpr A.Int)
      runParse "asdf" parseTypeExpr `shouldBe` expectAST 0 4 (A.makeValueExpr $ A.NamespacedIdentifier ["asdf"])
      runParse " asdf::foo " parseTypeExpr `shouldBe` expectAST 1 9 (A.makeValueExpr $ A.NamespacedIdentifier ["asdf", "foo"])
      runParse "&bool" parseTypeExpr `shouldBe` expectAST 0 5 (A.makeReferenceExpr A.Bool)
      runParse "&&bool" parseTypeExpr `shouldBe` Left ExpectedTypeExpr
    it "can parse basic expressions" $ do
      runParse "15" parseExpr `shouldBe` expectAST 0 2 (A.IntLit 15)
      runParse "void" parseExpr `shouldBe` expectAST 0 4 A.VoidLit
      runParse "true" parseExpr `shouldBe` expectAST 0 4 (A.BoolLit True)
      runParse "((15))" parseExpr `shouldBe` expectAST 0 6 (A.IntLit 15)
    it "can parse binary expressions" $ do
      runParse "true || false" parseExpr `shouldBe` expectAST 0 13 (A.BinaryOperator A.Or (buildAST 0 4 $ A.BoolLit True) (buildAST 8 5 $ A.BoolLit False))
      (show <$> runParse "true || false && true" parseExpr) `shouldBe` Right "(true || (false && true))"
      (show <$> runParse "true && true || false" parseExpr) `shouldBe` Right "((true && true) || false)"
      (show <$> runParse "1 * 2 + 3 * 4" parseExpr) `shouldBe` Right "((1 * 2) + (3 * 4))"
      (show <$> runParse "1 * 2 > 3 - 2 * 3 + 4" parseExpr) `shouldBe` Right "((1 * 2) > (3 - ((2 * 3) + 4)))"
    it "can parse unary expressions" $ do
      runParse "-+3" parseExpr `shouldBe` expectAST 0 3 (A.UnaryOperator A.Minus (buildAST 1 2 $ A.UnaryOperator A.Plus (buildAST 2 1 $ A.IntLit 3)))
      (show <$> runParse "15 + -3" parseExpr) `shouldBe` Right "(15 + (-3))"
      (show <$> runParse "!true || !false && !true" parseExpr) `shouldBe` Right "((!true) || ((!false) && (!true)))"
    it "can parse variable declarations" $ do
      runParse "let x: void = void" parseStmt `shouldBe` expectAST 0 18 (A.Let {name = buildAST 4 1 "x", type_ = buildAST 7 4 $ A.makeValueExpr A.Void, value = buildAST 14 4 A.VoidLit})
      (show <$> runParse "let x: foo::bar = true || false" parseStmt) `shouldBe` Right "let x: foo::bar = (true || false)"