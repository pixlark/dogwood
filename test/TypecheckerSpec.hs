{-# LANGUAGE QuasiQuotes #-}

module TypecheckerSpec where

import DW.Common
import DW.Error
import DW.Logging (noOpLogger, runLog)
import qualified DW.LowerPass as LowerPass
import qualified DW.Parser as Parser
import qualified DW.Typechecker as Typechecker
import DW.Util (stripCallStacks)
import Data.Text (show)
import NeatInterpolation
import Test.Hspec
import TestUtil
import Prelude hiding (show)

testTypecheck source = fmap stripCallStacks $ runEff $ runErrors $ runLog noOpLogger do
  -- Passes 1 and 2: Lexing and parsing
  ast <- runErrorAsErrors $ Parser.runParser source Parser.parseStmt
  -- Pass 3: Lowering
  let loweredAST = LowerPass.runLowerPass ast
  -- Pass 4: Typechecking
  runErrorAsErrors $ Typechecker.runTypechecker loweredAST

(<$$>) = fmap . fmap

spec :: SpecWith ()
spec = do
  describe "the Typechecker module" do
    it "typechecks basic expressions" do
      (show <$$> testTypecheck "let x: void = void;") `shouldReturn` Right "let x: void = void;"
      (show <$$> testTypecheck "let x: int = 1 + 2;") `shouldReturn` Right "let x: int = (1 + 2 : int);"
      (show <$$> testTypecheck "let x: bool = !false;") `shouldReturn` Right "let x: bool = (!false : bool);"
      (show <$$> testTypecheck "let x: int = 5 != 2;") `shouldSatisfyM` isErrorKind' (TypeMismatch "int" "bool")
    it "typechecks undefined expressions" do
      (show <$$> testTypecheck "let x: int = undefined;") `shouldReturn` Right "let x: int = undefined;"
      (show <$$> testTypecheck "let x: bool = undefined;") `shouldReturn` Right "let x: bool = undefined;"
    it "typechecks body expressions" do
      (show <$$> testTypecheck "{}") `shouldReturn` Right "{\n} : void"
      (show <$$> testTypecheck "{ 5; }") `shouldReturn` Right "{\n5;\n} : void"
      (show <$$> testTypecheck "{ 5 }") `shouldReturn` Right "{\n5\n} : int"
      (show <$$> testTypecheck "{ true; 5; }") `shouldReturn` Right "{\ntrue;\n5;\n} : void"
      (show <$$> testTypecheck "{ true; 5 }") `shouldReturn` Right "{\ntrue;\n5\n} : int"
      (show <$$> testTypecheck "{ true 5 }") `shouldReturn` Right "{\ntrue\n5\n} : int"
    it "typechecks bound variables" do
      (show <$$> testTypecheck "let x: int = foo;") `shouldSatisfyM` isErrorKind' (UnboundVariable "foo")
      (show <$$> testTypecheck "{let foo: int = 5; let bar: int = foo;}") `shouldReturn` Right "{\nlet foo: int = 5;\nlet bar: int = (foo : int);\n} : void"
    it "typechecks function calls" do
      (show <$$> testTypecheck "{let x: int = 5; x(15, 16);}") `shouldSatisfyM` isErrorKind' (CallingNonFunction "int")
      (show <$$> testTypecheck "{let f: fn(int, int) -> bool = undefined; f(5, 6)}") `shouldReturn` Right "{\nlet f: fn(int, int) -> bool = undefined;\n(f(5, 6) : bool)\n} : bool"
      (show <$$> testTypecheck "{let f: fn(int, int) -> bool = undefined; f(true, 6)}") `shouldSatisfyM` isErrorKind' (TypeMismatch "int" "bool")
      (show <$$> testTypecheck "{let f: fn(int, int) -> void = undefined; f(1)}") `shouldSatisfyM` isErrorKind' (WrongArgumentCount 2 1)
    it "typechecks if expressions" do
      (show <$$> testTypecheck "if true void else if false void") `shouldReturn` Right "(if true void else (if false void else void) : void) : void"
      (show <$$> testTypecheck "if 15 void else if false void") `shouldSatisfyM` isErrorKind' (TypeMismatch "bool" "int")
      (show <$$> testTypecheck "if true 15 else 16") `shouldReturn` Right "(if true 15 else 16) : int"
      (show <$$> testTypecheck "if true 15 else true") `shouldSatisfyM` isErrorKind' (TypeMismatch "int" "bool")
      (show <$$> testTypecheck "if true { 15; }") `shouldReturn` Right "(if true {\n15;\n} : void else void) : void"
      -- TODO: is this one reasonable behavior? maybe we should be okay with this, even though they didn't
      --       "discard" the value with a semicolon?
      --       what does rust do?
      (show <$$> testTypecheck "if true { 15 }") `shouldSatisfyM` isErrorKind' (TypeMismatch "int" "void")
    it "typechecks loops" do
      (show <$$> testTypecheck "loop { 15 }") `shouldSatisfyM` isErrorKind' (TypeMismatch "void" "int")
      (show <$$> testTypecheck "loop { 15; }") `shouldReturn` Right "loop {\n15;\n} : void"
    it "typechecks lexical scopes" do
      (show <$$> testTypecheck "{ let x: int = 5; { let x: bool = false; x || false } x + 5 }") `shouldReturn` Right "{\nlet x: int = 5;\n{\nlet x: bool = false;\n((x : bool) || false : bool)\n} : bool\n((x : int) + 5 : int)\n} : int"
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
              } : void else void) : void
              acc = ((acc : int) * (n : int) : int);
              n = ((n : int) - 1 : int);
              } : void
              } : void
            |]
      (show <$$> testTypecheck source) `shouldReturn` Right expected