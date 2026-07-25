{-# LANGUAGE QuasiQuotes #-}

module NameResolutionSpec (spec) where

import DW.Common
import DW.ConstExprPass qualified as ConstExprPass
import DW.Error
import DW.Logging (noOpLogger, runLog)
import DW.LowerPass qualified as LowerPass
import DW.NameResolutionPass qualified as NameResolutionPass
import DW.Parser qualified as Parser
import DW.Util (stripCallStacks)

import Data.Text qualified as Text
import NeatInterpolation
import Test.Hspec
import TestUtil

testNameResolution source =
  runEff $
    runLog noOpLogger $
      stripCallStacks <$> runErrors do
        ast <- Parser.runParser source Parser.parseTopLevel
        ConstExprPass.runConstExprPass ast
        let lst = LowerPass.runLowerPass ast
        NameResolutionPass.runNameResolution lst

(<$$>) = fmap . fmap

spec :: SpecWith ()
spec = do
  describe "the NameResolutionPass module" do
    it "reports unbound variables" do
      testNameResolution "let main = fn() { let x: int = foo; };"
        `shouldSatisfyM` isErrorKind (UnboundVariable "foo")

      testNameResolution "let main = fn() { foo = 5; };"
        `shouldSatisfyM` isErrorKind (UnboundVariable "foo")

    it "resolves bound variables" do
      (show <$$> testNameResolution "let main = fn() { let foo = 5; let bar = foo; };")
        `shouldReturn` Right "let main = fn() -> void {\nlet foo = 5;\nlet bar = foo;\n};\n"

    it "resolves variables in nested scopes" do
      (show <$$> testNameResolution "let main = fn() { let x = 5; { let y = x; }; };")
        `shouldReturn` Right "let main = fn() -> void {\nlet x = 5;\n{\nlet y = x;\n};\n};\n"

    it "handles variable shadowing" do
      (show <$$> testNameResolution "let main = fn() { let x = 5; { let x = true; let y = x; } let z = x; };")
        `shouldReturn` Right "let main = fn() -> void {\nlet x = 5;\n{\nlet x = true;\nlet y = x;\n}\nlet z = x;\n};\n"

    it "allows shadowing with reference to outer variable" do
      (show <$$> testNameResolution "let main = fn() { let x = 5; let x = x; };")
        `shouldReturn` Right "let main = fn() -> void {\nlet x = 5;\nlet x = x;\n};\n"

    it "does not allow lambdas to capture outer scope" do
      testNameResolution "let main = fn() { let x = 5; let f = fn() -> int: x; };"
        `shouldSatisfyM` isErrorKind (UnboundVariable "x")

    it "resolves lambda parameters" do
      (show <$$> testNameResolution "let main = fn() { let f = fn(x: int) -> int: x; };")
        `shouldReturn` Right "let main = fn() -> void {\nlet f = fn(x: int) -> int: x;\n};\n"

    it "reports duplicate parameter names" do
      testNameResolution "let main = fn() { let f = fn(x: int, x: int) -> int: x; };"
        `shouldSatisfyM` isErrorKind (DuplicateParameterNames "x")

    it "reports multiple definitions of top-level variables" do
      testNameResolution "let foo = 5; let foo = 6; let main = fn() {};"
        `shouldSatisfyM` isErrorKind (MultipleDefinitionsOfTLVariable "foo")

    it "resolves mutually recursive top-level functions" do
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

              let main = fn() {};
            |]
      (show <$$> testNameResolution source) `shouldSatisfyM` isRight

    it "allows top-level functions to call themselves recursively" do
      let source =
            [text|
              let factorial = fn(n: int) -> int {
                if n == 0
                  1
                  else n * factorial(n - 1)
              };

              let main = fn() {};
            |]
      (show <$$> testNameResolution source) `shouldSatisfyM` isRight
