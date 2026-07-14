module IntegrationSpec where

import DW.Common
import DW.Config (ConfigData (..), LogLevel (..))
import DW.Frontend qualified as Frontend
import DW.Util (orElse)

import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, takeFileName)
import System.Process (readProcessWithExitCode)
import Test.Hspec

integrationTest sourceFile = do
  let sourcePath = "test/integration_tests/" ++ sourceFile
  createDirectoryIfMissing False ".testdata"
  let outputFile = ".testdata/" ++ dropExtension (takeFileName sourcePath)
  let cfg =
        ConfigData
          { sourceFile = Text.pack sourcePath,
            outputFile = Just (Text.pack outputFile),
            logLevel = Quiet
          }
  exitCode <- Frontend.run cfg
  case exitCode of
    ExitFailure _ -> error "Compilation failed!"
    ExitSuccess -> do
      let expectationFilePath = dropExtension sourcePath ++ ".expect"
      expectationExists <- doesFileExist expectationFilePath
      expectedStdout <- if expectationExists then Just <$> Text.IO.readFile expectationFilePath else return Nothing

      let errorExpectationFilePath = dropExtension sourcePath ++ ".expecterr"
      errorExpectationExists <- doesFileExist errorExpectationFilePath
      expectedStderr <- if errorExpectationExists then Just <$> Text.IO.readFile errorExpectationFilePath else return Nothing

      (_, stdout, stderr) <- readProcessWithExitCode outputFile [] ""
      when (fmap (/= Text.pack stdout) expectedStdout `orElse` False) $ error "Unexpected output!"
      when (fmap (/= Text.pack stderr) expectedStderr `orElse` False) $ error "Unexpected output!"

      return (Text.pack stdout, Text.pack stderr)

spec = do
  describe "executables produced by the compiler run correctly" do
    it "can calculate the fibonacci sequence" do
      void $ integrationTest "fibonacci.pr"
    it "can calculate factorials" do
      void $ integrationTest "factorial.pr"
    it "can sum all the multiples of 3 and 5 below a thousand" do
      void $ integrationTest "multiplesof3and5.pr"
    it "can box and print function pointers" do
      (stdout, _) <- integrationTest "boxedfn.pr"
      stdout `shouldSatisfy` Text.isPrefixOf "fn(any) -> void"
    it "crashes when evaluating undefined" do
      void $ integrationTest "evaluateundefined.pr"
    it "handles variable shadowing" do
      void $ integrationTest "variableshadowing.pr"
    it "can define and use a basic function" do
      void $ integrationTest "basicfn.pr"
    it "can define and use a more complex function" do
      void $ integrationTest "complexfn.pr"
    it "can define and modify global variables" do
      void $ integrationTest "toplevelvalues.pr"
    it "can use mututally recursive functions" do
      void $ integrationTest "mutualrecursion.pr"
    it "can define and use a higher-order function" do
      void $ integrationTest "higherorderfn.pr"
