{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module TypecheckerSpec where

import AST (AST)
import qualified AST as A
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.State.Static.Local
import Error
import Parser (parseStmt, runParse)
import Test.Hspec
import Typechecker.Internal
import TypedAST (TST)
import qualified TypedAST as T

-- runTypecheck :: Text -> TST T.Stmt
-- runTypecheck source = runExceptT $ do
--   -- ast <- ExceptT $ runParse source parseStmt
--   (stmt, _) <- typecheckStmt (NE.fromList [[]]) ast
--   return stmt

runTypecheck :: Text -> Result (TST T.Stmt)
runTypecheck source = case result of
  Left e -> Left e
  Right a -> runPureEff $ runErrorNoCallStack $ evalState makeTypechecker $ typecheckStmt a
  where
    result = runParse source parseStmt

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
