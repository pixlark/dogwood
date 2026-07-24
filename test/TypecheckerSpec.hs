{-# LANGUAGE QuasiQuotes #-}

module TypecheckerSpec where

import DW.Common
import DW.ConstExprPass qualified as ConstExprPass
import DW.Error
import DW.Logging (noOpLogger, runLog)
import DW.LowerPass qualified as LowerPass
import DW.Parser qualified as Parser
import DW.Typechecker qualified as Typechecker
import DW.Util (stripCallStacks)

import Data.Text (show)
import Data.Text qualified as Text
import NeatInterpolation
import Test.Hspec
import TestUtil
import Prelude hiding (show)

testTypecheck source = fmap stripCallStacks $ runEff $ runErrors $ runLog noOpLogger do
  -- Passes 1 and 2: Lexing and parsing
  ast <- Parser.runParser source Parser.parseTopLevel
  -- Pass 3: Constexpr checking
  ConstExprPass.runConstExprPass ast
  -- Pass 4: Lowering
  let loweredAST = LowerPass.runLowerPass ast
  -- Pass 5: Typechecking
  Typechecker.runTypechecker loweredAST

(<$$>) = fmap . fmap

spec :: SpecWith ()
spec = do
  describe "the Typechecker module" do
    it "typechecks basic expressions" do
      (show <$$> testTypecheck "let main = fn() { let x: void = void; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet x: void = void;\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let x: int = 1 + 2; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet x: int = (1 + 2 : int);\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let x: bool = !false; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet x: bool = (!false : bool);\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let x: int = 5 != 2; };")
        `shouldSatisfyM` isErrorKind (TypeMismatch "int" "bool")

    it "typechecks body expressions" do
      (show <$$> testTypecheck "let main = fn() { {}; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\n{\n} : void;\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let x = { 5; }; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet x: void = {\n5;\n} : void;\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let x = { 5 }; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet x: int = {\n5\n} : int;\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let x = { true; 5; }; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet x: void = {\ntrue;\n5;\n} : void;\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let x = { true; 5 }; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet x: int = {\ntrue;\n5\n} : int;\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let x = { true 5 }; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet x: int = {\ntrue\n5\n} : int;\n} : void;\n"

    it "typechecks bound variables" do
      (show <$$> testTypecheck "let main = fn() { let x: int = foo; };")
        `shouldSatisfyM` isErrorKind (UnboundVariable "foo")

      (show <$$> testTypecheck "let main = fn() { {let foo: int = 5; let bar: int = foo;}; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\n{\nlet foo: int = 5;\nlet bar: int = (foo : int);\n} : void;\n} : void;\n"

    it "can infer the type of a variable declaration" do
      (show <$$> testTypecheck "let main = fn() { let x = 5; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet x: int = 5;\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let print = builtin print; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet print: fn(any) -> void = builtin print : fn(any) -> void;\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let x = 5; let y = x; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet x: int = 5;\nlet y: int = (x : int);\n} : void;\n"

    it "typechecks function calls" do
      (show <$$> testTypecheck "let main = fn() { let x: int = 5; x(15, 16); };")
        `shouldSatisfyM` isErrorKind (CallingNonFunction "int")

      (show <$$> testTypecheck "let main = fn() { let f = fn(a: int, b: int) -> bool: true; let y = f(5, 6); };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet f: fn(int, int) -> bool = fn(a: int, b: int) -> bool: true;\nlet y: bool = (f(5, 6) : bool);\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let f = fn(a: int, b: int) -> bool: true; f(true, 6); };")
        `shouldSatisfyM` isErrorKind (TypeMismatch "int" "bool")

      (show <$$> testTypecheck "let main = fn() { let f = fn(a: int, b: int) -> void {}; f(1); };")
        `shouldSatisfyM` isErrorKind (WrongArgumentCount 2 1)

    it "typechecks if expressions" do
      (show <$$> testTypecheck "let main = fn() { if true void else if false void; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\n(if true void else (if false void else void) : void) : void;\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { if 15 void else if false void; };")
        `shouldSatisfyM` isErrorKind (TypeMismatch "bool" "int")

      (show <$$> testTypecheck "let main = fn() { let x = if true 15 else 16; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet x: int = (if true 15 else 16) : int;\n} : void;\n"

      (show <$$> testTypecheck "let main = fn() { let x = if true 15 else true; };")
        `shouldSatisfyM` isErrorKind (TypeMismatch "int" "bool")

      (show <$$> testTypecheck "let main = fn() { if true { 15; }; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\n(if true {\n15;\n} : void else void) : void;\n} : void;\n"

      -- TODO: is this one reasonable behavior? maybe we should be okay with this, even though they didn't
      --       "discard" the value with a semicolon?
      --       what does rust do?
      (show <$$> testTypecheck "let main = fn() { if true { 15 }; };")
        `shouldSatisfyM` isErrorKind (TypeMismatch "int" "void")

    it "typechecks loops" do
      (show <$$> testTypecheck "let main = fn() { loop { 15 } };")
        `shouldSatisfyM` isErrorKind (TypeMismatch "void" "int")

      (show <$$> testTypecheck "let main = fn() { loop { 15; } };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nloop {\n15;\n} : void\n} : void;\n"

    it "typechecks lexical scopes" do
      (show <$$> testTypecheck "let main = fn() { let y = { let x: int = 5; { let x: bool = false; x || false } x + 5 }; };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet y: int = {\nlet x: int = 5;\n{\nlet x: bool = false;\n((x : bool) || false : bool)\n} : bool\n((x : int) + 5 : int)\n} : int;\n} : void;\n"

    it "typechecks lambda expressions" do
      (show <$$> testTypecheck "let main = fn() { let x = 5; let f = fn() -> int: x; };")
        `shouldSatisfyM` isErrorKind (UnboundVariable "x")

      (show <$$> testTypecheck "let main = fn() { let f = fn() -> int: void; };")
        `shouldSatisfyM` isErrorKind (TypeMismatch "void" "int")

      (show <$$> testTypecheck "let main = fn() { let f = fn(x: int) -> int: x; let y = f(1); };")
        `shouldReturn` Right "let main: fn() -> void = fn() -> void {\nlet f: fn(int) -> int = fn(x: int) -> int: (x : int);\nlet y: int = (f(1) : int);\n} : void;\n"

    it "typechecks mutually recursive toplevel functions" do
      let source =
            [text|
              let f = fn(x: int) -> int {
                if x > 0
                  g(x)
                  else x
              };

              let g = fn(x: int) -> int {
                f(x - 1)
              };

              let main = fn() {
                let print = builtin print;
                print(f(5));
              };
            |]
          expected =
            [text|
              let f: fn(int) -> int = fn(x: int) -> int {
              (if ((x : int) > 0 : bool) (g((x : int)) : int) else (x : int)) : int
              } : int;
              let g: fn(int) -> int = fn(x: int) -> int {
              (f(((x : int) - 1 : int)) : int)
              } : int;
              let main: fn() -> void = fn() -> void {
              let print: fn(any) -> void = builtin print : fn(any) -> void;
              (print((f(5) : int)) : void);
              } : void;
            |]
      (show <$$> testTypecheck source) `shouldReturn` Right (expected `Text.append` "\n")

    it "typechecks complex programs" do
      let source =
            [text|
              let main = fn() {
                let n: int = 5;
                let acc: int = 1;
                loop {
                  if n == 0 {
                    break;
                  }
                  acc = acc * n;
                  n = n - 1;
                }
              };
            |]
          expected =
            [text|
              let main: fn() -> void = fn() -> void {
              let n: int = 5;
              let acc: int = 1;
              loop {
              (if ((n : int) == 0 : bool) {
              break;
              } : void else void) : void
              acc = ((acc : int) * (n : int) : int);
              n = ((n : int) - 1 : int);
              } : void
              } : void;
            |]
      (show <$$> testTypecheck source) `shouldReturn` Right (expected `Text.append` "\n")
