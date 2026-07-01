module Clang where

import Common
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Effectful (IOE, MonadIO (liftIO))
import Error
import System.Exit (ExitCode (..))
import System.Process

compileExecutable :: (Error Err :> es, IOE :> es) => Text -> Eff es FilePath
compileExecutable source = do
  liftIO $ Text.IO.writeFile "generated.c" source
  exitCode <- liftIO $ withCreateProcess (proc "clang" ["generated.c", "-o", "a.out"]) $ \_ _ _ p -> waitForProcess p
  case exitCode of
    ExitSuccess -> return "a.out"
    ExitFailure _ -> throwSpan (Span 0 0) InternalCompilerError
