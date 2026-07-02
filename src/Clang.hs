module Clang where

import Common
import Config (Config, ConfigData (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.Directory
import System.Exit (ExitCode (..))
import System.Process
import Util (orElse)

compileExecutable :: (Error Err :> es, Config :> es, IOE :> es) => Text -> Eff es FilePath
compileExecutable source = do
  liftIO $ Text.IO.writeFile "generated.c" source
  runtimeDir <- liftIO $ makeAbsolute "runtime"
  cfg <- ask
  let outputFile = Text.unpack $ cfg.outputFile `orElse` "a.out"
  let clangArgs =
        [ "-g",
          "generated.c",
          "-o",
          outputFile,
          "-I",
          "runtime",
          "-L",
          "runtime",
          "-lruntime",
          "-Wl,-rpath," ++ runtimeDir
        ]
  exitCode <- liftIO $ withCreateProcess (proc "clang" clangArgs) $ \_ _ _ p -> waitForProcess p
  case exitCode of
    ExitSuccess -> return outputFile
    ExitFailure _ -> throwSpan (Span 0 0) InternalCompilerError
