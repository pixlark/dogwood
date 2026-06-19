module TypecheckerSpec where

import AST (AST)
import qualified AST as A
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Parser (parseStmt, runParse)
import Test.Hspec
import Typechecker.Internal
import TypedAST (TST)
import qualified TypedAST as T

runTypecheck :: Text -> TST T.Stmt
runTypecheck source = runExceptT $ do
  -- ast <- ExceptT $ runParse source parseStmt
  (stmt, _) <- typecheckStmt (NE.fromList [[]]) ast
  return stmt

spec = do
  describe "the Typechecker module" $ do
    it "typechecks basic expressions" $ do
      1 `shouldBe` 1
