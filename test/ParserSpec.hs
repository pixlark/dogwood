{-# LANGUAGE QuasiQuotes #-}

module ParserSpec (spec) where

import DW.AST (AST (..))
import DW.AST qualified as A
import DW.Common
import DW.Error
import DW.Lexer.Internal
import DW.Parser.Internal
import DW.Util (stripCallStacks)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import NeatInterpolation
import Test.Hspec

consumeSymbol = do
  current <- gets DW.Parser.Internal.current
  case current.kind of
    Symbol sym -> do DW.Parser.Internal.advance; return $ Just sym
    _ -> return Nothing

parseCommas trailing = parseSeparatedSequence SeparatorConfig {trailing, separator = Glyph ",", consume = consumeSymbol}

buildAST :: Int -> Int -> a -> AST a
buildAST start length inner = AST inner (Span start length)

expectAST :: Int -> Int -> a -> Result (AST a)
expectAST start length inner = Right $ buildAST start length inner

testParse :: (HasCallStack) => Text -> Eff '[State Parser, Errors Err] a -> Result a
testParse source f = stripCallStacks $ runPureEff $ runErrors $ runParser source f

spec =
  describe "the Parser module" do
    it "can parse separated sequences" do
      testParse "a, b, c" (parseCommas False) `shouldBe` Right ["a", "b", "c"]
      testParse "a, b, c," (parseCommas False) `shouldSatisfy` isErrorKind ExpectedAnotherElementOfSequence
      testParse "a, b, c," (parseCommas True) `shouldBe` Right ["a", "b", "c"]
      testParse "a,, b, c" (parseCommas False) `shouldSatisfy` isErrorKind ExpectedAnotherElementOfSequence
      testParse "a" (parseCommas True) `shouldBe` Right ["a"]
      testParse "" (parseCommas True) `shouldBe` Right []
    it "can parse type expressions" do
      testParse "void" parseTypeExpr `shouldBe` expectAST 0 4 (A.makeValueExpr A.Void)
      testParse "bool" parseTypeExpr `shouldBe` expectAST 0 4 (A.makeValueExpr A.Bool)
      testParse "int" parseTypeExpr `shouldBe` expectAST 0 3 (A.makeValueExpr A.Int)
      testParse "asdf" parseTypeExpr `shouldBe` expectAST 0 4 (A.makeValueExpr $ A.NamespacedIdentifier ["asdf"])
      testParse " asdf::foo " parseTypeExpr `shouldBe` expectAST 1 9 (A.makeValueExpr $ A.NamespacedIdentifier ["asdf", "foo"])
      testParse "&bool" parseTypeExpr `shouldBe` expectAST 0 5 (A.makeReferenceExpr A.Bool)
      testParse "&&bool" parseTypeExpr `shouldSatisfy` isErrorKind ExpectedTypeExpr
      (show <$> testParse "fn() -> void" parseTypeExpr) `shouldBe` Right "fn() -> void"
      (show <$> testParse "fn(int) -> int" parseTypeExpr) `shouldBe` Right "fn(int) -> int"
      (show <$> testParse "fn(int, bool) -> void" parseTypeExpr) `shouldBe` Right "fn(int, bool) -> void"
    describe "can parse expressions" do
      it "can parse basic expressions" do
        testParse "15" parseExpr `shouldBe` expectAST 0 2 (A.IntLit 15)
        testParse "void" parseExpr `shouldBe` expectAST 0 4 A.VoidLit
        testParse "true" parseExpr `shouldBe` expectAST 0 4 (A.BoolLit True)
        testParse "((15))" parseExpr `shouldBe` expectAST 0 6 (A.IntLit 15)
      it "can parse binary expressions" do
        testParse "true || false" parseExpr `shouldBe` expectAST 0 13 (A.BinaryOperator A.Or (buildAST 0 4 $ A.BoolLit True) (buildAST 8 5 $ A.BoolLit False))
        (show <$> testParse "true || false && true" parseExpr) `shouldBe` Right "(true || (false && true))"
        (show <$> testParse "true && true || false" parseExpr) `shouldBe` Right "((true && true) || false)"
        (show <$> testParse "1 * 2 + 3 * 4" parseExpr) `shouldBe` Right "((1 * 2) + (3 * 4))"
        (show <$> testParse "1 * 2 > 3 - 2 * 3 + 4" parseExpr) `shouldBe` Right "((1 * 2) > (3 - ((2 * 3) + 4)))"
      it "can parse unary expressions" do
        testParse "-+3" parseExpr `shouldBe` expectAST 0 3 (A.UnaryOperator A.Minus (buildAST 1 2 $ A.UnaryOperator A.Plus (buildAST 2 1 $ A.IntLit 3)))
        (show <$> testParse "15 + -3" parseExpr) `shouldBe` Right "(15 + (-3))"
        (show <$> testParse "!true || !false && !true" parseExpr) `shouldBe` Right "((!true) || ((!false) && (!true)))"
      it "can parse if expressions" do
        testParse "if true { 4 } else { 5 }" parseExpr
          `shouldBe` expectAST
            0
            24
            ( A.IfChain
                (NE.fromList [(buildAST 3 4 $ A.BoolLit True, buildAST 8 5 (A.ExprBody $ A.Body [buildAST 10 1 $ A.ExprStmt (buildAST 10 1 $ A.IntLit 4) False]))])
                (Just $ buildAST 19 5 (A.ExprBody $ A.Body [buildAST 21 1 $ A.ExprStmt (buildAST 21 1 $ A.IntLit 5) False]))
            )
        (show <$> testParse "if true && false { return 5; } else if false { 6 }" parseExpr) `shouldBe` Right "if (true && false) {\nreturn 5;\n} else if false {\n6\n}"
        (show <$> testParse "if true false else if false true else 1500" parseExpr) `shouldBe` Right "if true false else if false true else 1500"
        (show <$> testParse "if true false" parseExpr) `shouldBe` Right "if true false"
      it "can parse function calls" do
        (show <$> testParse "foo(1, 2)" parseExpr) `shouldBe` Right "foo(1, 2)"
        (show <$> testParse "foo(1, 2,)" parseExpr) `shouldBe` Right "foo(1, 2)"
        (show <$> testParse "foo(1, 2,,)" parseExpr) `shouldSatisfy` isErrorKind ExpectedExpr
        (show <$> testParse "(1 + 2)(5)" parseExpr) `shouldBe` Right "((1 + 2))(5)"
        (show <$> testParse "{let x: int = 5; x(15, 16);}" parseStmt) `shouldBe` Right "{\nlet x: int = 5;\nx(15, 16);\n}"
      it "can parse statement bodies" do
        (show <$> testParse "{ let x: int = 5; 15 }" parseExpr) `shouldBe` Right "{\nlet x: int = 5;\n15\n}"
        (show <$> testParse "{ let x: int = 5; 15; }" parseExpr) `shouldBe` Right "{\nlet x: int = 5;\n15;\n}"
        (show <$> testParse "{ break; 5; void }" parseExpr) `shouldBe` Right "{\nbreak;\n5;\nvoid\n}"
        (show <$> testParse "{ break 5 }" parseExpr) `shouldSatisfy` isErrorKind (ExpectedGlyph ";")
      -- TODO: right now this fails because we accept statement expressions without a semicolon
      --       is that okay? maybe we don't care
      -- (show <$> testParse "{ 6 5 }" parseExpr) `shouldSatisfy` isErrorKind (ExpectedGlyph ";")
      it "can omit semicolon after certain expressions" do
        (show <$> testParse "{ if true {} false }" parseExpr) `shouldBe` Right "{\nif true {\n}\nfalse\n}"
      it "can parse lambdas" do
        (show <$> testParse "let f = fn(x: int, y: bool) -> bool: true;" parseStmt) `shouldBe` Right "let f = fn(x: int, y: bool) -> bool: true;"
        (show <$> testParse "let f = fn() -> bool: true;" parseStmt) `shouldBe` Right "let f = fn() -> bool: true;"
        (show <$> testParse "let f = fn(x: int) {};" parseStmt) `shouldBe` Right "let f = fn(x: int) {\n};"
        (show <$> testParse "let f = fn() {};" parseStmt) `shouldBe` Right "let f = fn() {\n};"
        (show <$> testParse "let f = fn() -> int: 5;" parseStmt) `shouldBe` Right "let f = fn() -> int: 5;"
    describe "can parse statements" do
      it "can parse basic statements" do
        (show <$> testParse "return;" parseStmt) `shouldBe` Right "return;"
        testParse "return" parseStmt `shouldSatisfy` isErrorKind ExpectedExpr
        (show <$> testParse "return void;" parseStmt) `shouldBe` Right "return void;"
        testParse "return void" parseStmt `shouldSatisfy` isErrorKind (ExpectedGlyph ";")
        (show <$> testParse "break;" parseStmt) `shouldBe` Right "break;"
        testParse "break" parseStmt `shouldSatisfy` isErrorKind (ExpectedGlyph ";")
      it "can parse variable declarations" do
        testParse "let x: void = void;" parseStmt `shouldBe` expectAST 0 18 (A.Let {name = buildAST 4 1 "x", type_ = Just $ buildAST 7 4 $ A.makeValueExpr A.Void, value = buildAST 14 4 A.VoidLit})
        (show <$> testParse "let x: foo::bar = true || false;" parseStmt) `shouldBe` Right "let x: foo::bar = (true || false);"
      it "can parse variable assignments" do
        testParse "f = 5;" parseStmt `shouldBe` expectAST 0 6 (A.Assign {lvalue = buildAST 0 1 $ A.LVariable "f", value = buildAST 4 1 $ A.IntLit 5})
        -- parsing variable assignment requires speculative lookahead, and if it fails it falls back to parsing a statement-level expression
        -- so make sure that works
        testParse "5" parseStmt `shouldBe` expectAST 0 1 (A.ExprStmt {value = buildAST 0 1 $ A.IntLit 5, semicolon = False})
        (show <$> testParse "asdf = 5 + 6;" parseStmt) `shouldBe` Right "asdf = (5 + 6);"
      it "can parse loop statements" do
        (show <$> testParse "loop { x = 5; break; }" parseStmt) `shouldBe` Right "loop {\nx = 5;\nbreak;\n}"
    it "can parse toplevel statements" do
      (show <$> testParse "let x = 5;\nlet y = 10;\n" parseTopLevel) `shouldBe` Right "let x = 5;\nlet y = 10;\n"
    describe "can parse complex programs" do
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
       in it "can parse a factorial program" do
            (show <$> testParse source parseExpr) `shouldBe` Right (T.unpack expected)
