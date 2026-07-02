module Frontend (run) where

import qualified Clang
import Common (liftIO, runEff, runError, runReader)
import qualified Compiler
import Config (ConfigData (..), LogLevel (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Effectful.Error.Static (prettyCallStack)
import qualified EmitC
import Error (displayError)
import Logging (noOpLogger, runLog, standardLoggerWithIgnoredFunctions)
import qualified LoopPass
import qualified LowerPass
import qualified Parser
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import qualified Typechecker

run :: ConfigData -> IO ExitCode
run cfg = do
  source <- Text.IO.readFile (Text.unpack cfg.sourceFile)

  let logger = if cfg.logLevel == Loud then standardLoggerWithIgnoredFunctions ignoredFunctions else noOpLogger

  let write str = if cfg.logLevel == Quiet then return () else putStrLn str

  executableName <- runEff $ runLog logger $ runError $ runReader cfg $ do
    -- Passes 1 and 2: Lexing and parsing
    liftIO $ write "Lexing and parsing..."
    ast <- Parser.runParser source Parser.parseStmt
    -- Pass 3: Lowering
    liftIO $ write "Lowering AST..."
    let loweredAST = LowerPass.runLowerPass ast
    -- Pass 4: Typechecking
    liftIO $ write "Typechecking AST..."
    typedAST <- Typechecker.runTypechecker loweredAST
    -- Pass 5: Loop validation
    liftIO $ write "Validating loops..."
    LoopPass.runLoopPass typedAST
    -- Pass 6: Compile to IR
    liftIO $ write "Compiling to IR..."
    program <- Compiler.runCompiler typedAST
    -- Pass 7: Generate C
    liftIO $ write "Generating C..."
    generatedC <- EmitC.runEmitC program
    -- Pass 8: Compile C with clang
    liftIO $ write "Compiling with clang..."
    executableName <- Clang.compileExecutable generatedC

    {- HLINT ignore -}
    return executableName

  exitCode <- case executableName of
    Left (callstack, e) -> do
      write $ prettyCallStack callstack
      write $ Text.unpack $ displayError source e
      return (ExitFailure 1)
    Right executableName -> do
      write $ "Compiled successfully into " ++ executableName
      return ExitSuccess

  return exitCode
  where
    -- functions that we want to ignore when marking the current function in the logging framework
    ignoredFunctions = ["markSealed"]