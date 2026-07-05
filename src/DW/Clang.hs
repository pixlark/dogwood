module DW.Clang where

import DW.Common
import DW.Config (Config, ConfigData (..))
import DW.Util (orElse)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.Directory
import System.Exit (ExitCode (..))
import System.Process

compileExecutable :: (Errors Err :> es, Config :> es, IOE :> es) => Text -> Eff es FilePath
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
