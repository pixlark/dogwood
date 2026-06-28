{-# LANGUAGE QuasiQuotes #-}

module TypecheckerSpec where

import Common
import Data.Text (show)
import Error
import LowerPass (runLowerPass)
import NeatInterpolation
import Parser (parseStmt, runParse)
import Test.Hspec
import Typechecker.Internal
import TypedAST (TST (..))
import qualified TypedAST as T
import Prelude hiding (show)

-- | Helper to run the full parse -> lower -> typecheck pipeline
testTypecheck :: Text -> Result (TST T.Stmt)
testTypecheck source = do
  ast <- runParse source parseStmt
  runTypecheck (runLowerPass ast)

spec :: SpecWith ()
spec = do
  describe "the Typechecker module" do
    it "typechecks basic expressions" do
      (show <$> testTypecheck "let x: void = void;") `shouldBe` Right "let x: void = void;"
      (show <$> testTypecheck "let x: int = 1 + 2;") `shouldBe` Right "let x: int = (1 + 2 : int);"
      (show <$> testTypecheck "let x: bool = !false;") `shouldBe` Right "let x: bool = (!false : bool);"
      (show <$> testTypecheck "let x: int = 5 != 2;") `shouldSatisfy` isErrorKind (TypeMismatch "int" "bool")
    it "typechecks undefined expressions" do
      (show <$> testTypecheck "let x: int = undefined;") `shouldBe` Right "let x: int = undefined;"
      (show <$> testTypecheck "let x: bool = undefined;") `shouldBe` Right "let x: bool = undefined;"
    it "typechecks body expressions" do
      (show <$> testTypecheck "{}") `shouldBe` Right "{\n} : void"
      (show <$> testTypecheck "{ 5; }") `shouldBe` Right "{\n5;\n} : void"
      (show <$> testTypecheck "{ 5 }") `shouldBe` Right "{\n5\n} : int"
      (show <$> testTypecheck "{ true; 5; }") `shouldBe` Right "{\ntrue;\n5;\n} : void"
      (show <$> testTypecheck "{ true; 5 }") `shouldBe` Right "{\ntrue;\n5\n} : int"
      (show <$> testTypecheck "{ true 5 }") `shouldBe` Right "{\ntrue\n5\n} : int"
    it "typechecks bound variables" do
      (show <$> testTypecheck "let x: int = foo;") `shouldSatisfy` isErrorKind (UnboundVariable "foo")
      (show <$> testTypecheck "{let foo: int = 5; let bar: int = foo;}") `shouldBe` Right "{\nlet foo: int = 5;\nlet bar: int = (foo : int);\n} : void"
    it "typechecks function calls" do
      (show <$> testTypecheck "{let x: int = 5; x(15, 16);}") `shouldSatisfy` isErrorKind (CallingNonFunction "int")
      (show <$> testTypecheck "{let f: fn(int, int) -> bool = undefined; f(5, 6)}") `shouldBe` Right "{\nlet f: fn(int, int) -> bool = undefined;\n(f(5, 6) : bool)\n} : bool"
      (show <$> testTypecheck "{let f: fn(int, int) -> bool = undefined; f(true, 6)}") `shouldSatisfy` isErrorKind (TypeMismatch "int" "bool")
      (show <$> testTypecheck "{let f: fn(int, int) -> void = undefined; f(1)}") `shouldSatisfy` isErrorKind (WrongArgumentCount 2 1)
    it "typechecks if expressions" do
      (show <$> testTypecheck "if true void else if false void") `shouldBe` Right "(if true void else if false void) : void"
      (show <$> testTypecheck "if 15 void else if false void") `shouldSatisfy` isErrorKind (TypeMismatch "bool" "int")
      (show <$> testTypecheck "if true 15 else 16") `shouldBe` Right "(if true 15 else 16) : int"
      (show <$> testTypecheck "if true 15 else true") `shouldSatisfy` isErrorKind (TypeMismatch "int" "bool")
      (show <$> testTypecheck "if true { 15; }") `shouldBe` Right "(if true {\n15;\n} : void) : void"
      -- TODO: is this one reasonable behavior? maybe we should be okay with this, even though they didn't
      --       "discard" the value with a semicolon?
      --       what does rust do?
      (show <$> testTypecheck "if true { 15 }") `shouldSatisfy` isErrorKind (TypeMismatch "void" "int")
    it "typechecks loops" do
      (show <$> testTypecheck "loop { 15 }") `shouldSatisfy` isErrorKind (TypeMismatch "void" "int")
      (show <$> testTypecheck "loop { 15; }") `shouldBe` Right "loop {\n15;\n} : void"
    it "typechecks lexical scopes" do
      (show <$> testTypecheck "{ let x: int = 5; { let x: bool = false; x || false } x + 5 }") `shouldBe` Right "{\nlet x: int = 5;\n{\nlet x: bool = false;\n((x : bool) || false : bool)\n} : bool\n((x : int) + 5 : int)\n} : int"
    it "typchecks complex programs" do
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
              (if ((n : int) == 0 : bool) {
              break;
              } : void) : void
              acc = ((acc : int) * (n : int) : int);
              n = ((n : int) - 1 : int);
              } : void
              } : void
            |]
      (show <$> testTypecheck source) `shouldBe` Right expected