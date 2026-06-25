{-# LANGUAGE QuasiQuotes #-}

module TypecheckerSpec where

import Common
import Data.Text (show)
import Error
import NeatInterpolation
import Test.Hspec
import Typechecker.Internal
import Prelude hiding (show)

spec :: SpecWith ()
spec = do
  describe "the Typechecker module" do
    it "typechecks basic expressions" do
      (show <$> runTypecheck "let x: void = void;") `shouldBe` Right "let x: void = void;"
      (show <$> runTypecheck "let x: int = 1 + 2;") `shouldBe` Right "let x: int = (1 + 2 : int);"
      (show <$> runTypecheck "let x: bool = !false;") `shouldBe` Right "let x: bool = (!false : bool);"
      (show <$> runTypecheck "let x: int = 5 != 2;") `shouldSatisfy` isErrorKind (TypeMismatch "int" "bool")
    it "typechecks undefined expressions" do
      (show <$> runTypecheck "let x: int = undefined;") `shouldBe` Right "let x: int = undefined;"
      (show <$> runTypecheck "let x: bool = undefined;") `shouldBe` Right "let x: bool = undefined;"
    it "typechecks body expressions" do
      (show <$> runTypecheck "{}") `shouldBe` Right "{\n} : void"
      (show <$> runTypecheck "{ 5; }") `shouldBe` Right "{\n5;\n} : void"
      (show <$> runTypecheck "{ 5 }") `shouldBe` Right "{\n5\n} : int"
      (show <$> runTypecheck "{ true; 5; }") `shouldBe` Right "{\ntrue;\n5;\n} : void"
      (show <$> runTypecheck "{ true; 5 }") `shouldBe` Right "{\ntrue;\n5\n} : int"
      (show <$> runTypecheck "{ true 5 }") `shouldBe` Right "{\ntrue\n5\n} : int"
    it "typechecks bound variables" do
      (show <$> runTypecheck "let x: int = foo;") `shouldSatisfy` isErrorKind (UnboundVariable "foo")
      (show <$> runTypecheck "{let foo: int = 5; let bar: int = foo;}") `shouldBe` Right "{\nlet foo: int = 5;\nlet bar: int = (foo : int);\n} : void"
    it "typechecks function calls" do
      (show <$> runTypecheck "{let x: int = 5; x(15, 16);}") `shouldSatisfy` isErrorKind (CallingNonFunction "int")
      (show <$> runTypecheck "{let f: fn(int, int) -> bool = undefined; f(5, 6)}") `shouldBe` Right "{\nlet f: fn(int, int) -> bool = undefined;\n(f(5, 6) : bool)\n} : bool"
      (show <$> runTypecheck "{let f: fn(int, int) -> bool = undefined; f(true, 6)}") `shouldSatisfy` isErrorKind (TypeMismatch "int" "bool")
      (show <$> runTypecheck "{let f: fn(int, int) -> void = undefined; f(1)}") `shouldSatisfy` isErrorKind (WrongArgumentCount 2 1)
    it "typechecks if expressions" do
      (show <$> runTypecheck "if true void else if false void") `shouldBe` Right "(if true void else if false void) : void"
      (show <$> runTypecheck "if 15 void else if false void") `shouldSatisfy` isErrorKind (TypeMismatch "bool" "int")
      (show <$> runTypecheck "if true 15 else 16") `shouldBe` Right "(if true 15 else 16) : int"
      (show <$> runTypecheck "if true 15 else true") `shouldSatisfy` isErrorKind (TypeMismatch "int" "bool")
      (show <$> runTypecheck "if true { 15; }") `shouldBe` Right "(if true {\n15;\n} : void) : void"
      -- TODO: is this one reasonable behavior? maybe we should be okay with this, even though they didn't
      --       "discard" the value with a semicolon?
      --       what does rust do?
      (show <$> runTypecheck "if true { 15 }") `shouldSatisfy` isErrorKind (TypeMismatch "void" "int")
    it "typechecks loops" do
      (show <$> runTypecheck "loop { 15 }") `shouldSatisfy` isErrorKind (TypeMismatch "void" "int")
      (show <$> runTypecheck "loop { 15; }") `shouldBe` Right "loop {\n15;\n} : void"
    it "typechecks lexical scopes" do
      (show <$> runTypecheck "let x: int = 5; { let x: bool = false; x || false } x + 5") `shouldBe` Right "let x: int = 5;\n{\nlet x: bool = false;\n(x || false : bool)\n} : bool\n(x + 5 : int)"
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
      (show <$> runTypecheck source) `shouldBe` Right expected