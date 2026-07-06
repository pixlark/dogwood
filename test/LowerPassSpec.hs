module LowerPassSpec (spec) where

import DW.Common
import DW.ConstExprPass qualified as ConstExprPass
import DW.Logging (noOpLogger, runLog)
import DW.LowerPass
import DW.Parser (parseTopLevel, runParser)
import DW.Util (leftMap, stripCallStacks)
import Data.Text qualified as Text
import Test.Hspec

testLowerPass :: Text -> IO (Result Text)
testLowerPass source =
  runEff $
    runLog noOpLogger $
      stripCallStacks <$> runErrors do
        ast <- runParser source parseTopLevel
        ConstExprPass.runConstExprPass ast
        let loweredAST = runLowerPass ast
        return $ Text.show loweredAST

spec :: SpecWith ()
spec = do
  describe "the LowerPass module" do
    it "lowers omitted lambda return types" do
      testLowerPass "let f = fn() {};" `shouldReturn` Right "let f = fn() -> void {\n};"
      testLowerPass "let f = fn(x: int): void;" `shouldReturn` Right "let f = fn(x: int) -> void: void;"
