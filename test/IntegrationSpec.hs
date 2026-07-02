module IntegrationSpec where

import Common
import Config (ConfigData (..), LogLevel (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Frontend
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, takeFileName)
import System.Process (readProcess)
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
      expectedOutput <- Text.IO.readFile expectationFilePath
      result <- Text.pack <$> readProcess outputFile [] ""
      when (result /= expectedOutput) $ error "Unexpected output!"

spec = do
  describe "executables produced by the compiler run correctly" do
    it "can calculate the fibonacci sequence" do
      integrationTest "fibonacci.pr"
    it "can calculate factorials" do
      integrationTest "factorial.pr"
    it "can sum all the multiples of 3 and 5 below a thousand" do
      integrationTest "multiplesof3and5.pr"
