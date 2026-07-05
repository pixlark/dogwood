module LowerPassSpec (spec) where

import DW.Common
import DW.LowerPass
import DW.Parser (parseStmt, runParser)
import DW.Util (stripCallStacks)

import Data.Text qualified as Text
import Test.Hspec

testLowerPass :: Text -> Result Text
testLowerPass source = stripCallStacks $ runPureEff $ runErrors do
  ast <- runParser source parseStmt
  let loweredAST = runLowerPass ast
  return $ Text.show loweredAST

spec :: SpecWith ()
spec = do
  describe "the LowerPass module" do
    it "lowers omitted lambda return types" do
      testLowerPass "let f = fn() {};" `shouldBe` Right "let f = fn() -> void {\n};"
      testLowerPass "let f = fn(x: int): void;" `shouldBe` Right "let f = fn(x: int) -> void: void;"
