{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParserSpec (spec) where

import AST (AST (..))
import qualified AST as A
-- import Control.Monad.Except
-- import Control.Monad.Loops
-- import Control.Monad.State.Lazy
import Data.Either
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Error
import Lexer.Internal
import NeatInterpolation
import Parser.Internal
import Test.Hspec

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

spec =
  describe "the Parser module" $ do
    it "can parse separated sequences" $ do
      runParse "a, b, c" (parseCommas False) `shouldBe` Right ["a", "b", "c"]
      runParse "a, b, c," (parseCommas False) `shouldSatisfy` isErrorKind ExpectedAnotherElementOfSequence
      runParse "a, b, c," (parseCommas True) `shouldBe` Right ["a", "b", "c"]
      runParse "a,, b, c" (parseCommas False) `shouldSatisfy` isErrorKind ExpectedAnotherElementOfSequence
      runParse "a" (parseCommas True) `shouldBe` Right ["a"]
      runParse "" (parseCommas True) `shouldBe` Right []
    it "can parse type expressions" $ do
      runParse "void" parseTypeExpr `shouldBe` expectAST 0 4 (A.makeValueExpr A.Void)
      runParse "bool" parseTypeExpr `shouldBe` expectAST 0 4 (A.makeValueExpr A.Bool)
      runParse "int" parseTypeExpr `shouldBe` expectAST 0 3 (A.makeValueExpr A.Int)
      runParse "asdf" parseTypeExpr `shouldBe` expectAST 0 4 (A.makeValueExpr $ A.NamespacedIdentifier ["asdf"])
      runParse " asdf::foo " parseTypeExpr `shouldBe` expectAST 1 9 (A.makeValueExpr $ A.NamespacedIdentifier ["asdf", "foo"])
      runParse "&bool" parseTypeExpr `shouldBe` expectAST 0 5 (A.makeReferenceExpr A.Bool)
      runParse "&&bool" parseTypeExpr `shouldSatisfy` isErrorKind ExpectedTypeExpr
      (show <$> runParse "fn() -> void" parseTypeExpr) `shouldBe` Right "fn() -> void"
      (show <$> runParse "fn(int) -> int" parseTypeExpr) `shouldBe` Right "fn(int) -> int"
      (show <$> runParse "fn(int, bool) -> void" parseTypeExpr) `shouldBe` Right "fn(int, bool) -> void"
    describe "can parse expressions" $ do
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
      it "can parse if expressions" $ do
        runParse "if true { 4 } else { 5 }" parseExpr
          `shouldBe` expectAST
            0
            24
            ( A.IfChain
                (NE.fromList [(buildAST 3 4 $ A.BoolLit True, buildAST 8 5 (A.ExprBody $ A.Body [buildAST 10 1 $ A.ExprStmt (buildAST 10 1 $ A.IntLit 4) False]))])
                (Just $ buildAST 19 5 (A.ExprBody $ A.Body [buildAST 21 1 $ A.ExprStmt (buildAST 21 1 $ A.IntLit 5) False]))
            )
        (show <$> runParse "if true && false { return 5; } else if false { 6 }" parseExpr) `shouldBe` Right "if (true && false) {\nreturn 5;\n} else if false {\n6\n}"
        (show <$> runParse "if true false else if false true else 1500" parseExpr) `shouldBe` Right "if true false else if false true else 1500"
        (show <$> runParse "if true false" parseExpr) `shouldBe` Right "if true false"
      it "can parse function calls" $ do
        (show <$> runParse "foo(1, 2)" parseExpr) `shouldBe` Right "foo(1, 2)"
        (show <$> runParse "foo(1, 2,)" parseExpr) `shouldBe` Right "foo(1, 2)"
        (show <$> runParse "foo(1, 2,,)" parseExpr) `shouldSatisfy` isErrorKind ExpectedExpr
        (show <$> runParse "(1 + 2)(5)" parseExpr) `shouldBe` Right "((1 + 2))(5)"
        (show <$> runParse "{let x: int = 5; x(15, 16);}" parseStmt) `shouldBe` Right "{\nlet x: int = 5;\nx(15, 16);\n}"
      it "can parse statement bodies" $ do
        (show <$> runParse "{ let x: int = 5; 15 }" parseExpr) `shouldBe` Right "{\nlet x: int = 5;\n15\n}"
        (show <$> runParse "{ let x: int = 5; 15; }" parseExpr) `shouldBe` Right "{\nlet x: int = 5;\n15;\n}"
        (show <$> runParse "{ break; 5; void }" parseExpr) `shouldBe` Right "{\nbreak;\n5;\nvoid\n}"
        (show <$> runParse "{ break 5 }" parseExpr) `shouldSatisfy` isErrorKind (ExpectedGlyph ";")
      -- TODO: right now this fails because we accept statement expressions without a semicolon
      --       is that okay? maybe we don't care
      -- (show <$> runParse "{ 6 5 }" parseExpr) `shouldSatisfy` isErrorKind (ExpectedGlyph ";")
      it "can omit semicolon after certain expressions" $ do
        (show <$> runParse "{ if true {} false }" parseExpr) `shouldBe` Right "{\nif true {\n}\nfalse\n}"
    describe "can parse statements" $ do
      it "can parse basic statements" $ do
        (show <$> runParse "return;" parseStmt) `shouldBe` Right "return;"
        runParse "return" parseStmt `shouldSatisfy` isErrorKind ExpectedExpr
        (show <$> runParse "return void;" parseStmt) `shouldBe` Right "return void;"
        runParse "return void" parseStmt `shouldSatisfy` isErrorKind (ExpectedGlyph ";")
        (show <$> runParse "break;" parseStmt) `shouldBe` Right "break;"
        runParse "break" parseStmt `shouldSatisfy` isErrorKind (ExpectedGlyph ";")
      it "can parse variable declarations" $ do
        runParse "let x: void = void;" parseStmt `shouldBe` expectAST 0 18 (A.Let {name = buildAST 4 1 "x", type_ = buildAST 7 4 $ A.makeValueExpr A.Void, value = buildAST 14 4 A.VoidLit})
        (show <$> runParse "let x: foo::bar = true || false;" parseStmt) `shouldBe` Right "let x: foo::bar = (true || false);"
      it "can parse variable assignments" $ do
        runParse "f = 5;" parseStmt `shouldBe` expectAST 0 6 (A.Assign {lvalue = buildAST 0 1 $ A.LVariable "f", value = buildAST 4 1 $ A.IntLit 5})
        -- parsing variable assignment requires speculative lookahead, and if it fails it falls back to parsing a statement-level expression
        -- so make sure that works
        runParse "5" parseStmt `shouldBe` expectAST 0 1 (A.ExprStmt {value = buildAST 0 1 $ A.IntLit 5, semicolon = False})
        (show <$> runParse "asdf = 5 + 6;" parseStmt) `shouldBe` Right "asdf = (5 + 6);"
      it "can parse loop statements" $ do
        (show <$> runParse "loop { x = 5; break; }" parseStmt) `shouldBe` Right "loop {\nx = 5;\nbreak;\n}"
    describe "can parse complex programs" $ do
      let source =
            [text|
              {
                let n: int = 5;
                let acc: int = 1;
                loop {
                  if n == 0 {
                    break;
                  }
                  acc = acc * n;
                  n = n - 1;
                }
              }
            |]
          expected =
            [text|
              {
              let n: int = 5;
              let acc: int = 1;
              loop {
              if (n == 0) {
              break;
              }
              acc = (acc * n);
              n = (n - 1);
              }
              }
            |]
       in it "can parse a factorial program" $ do
            (show <$> runParse source parseExpr) `shouldBe` Right (T.unpack expected)
