module DW.Frontend (run) where

import qualified DW.Clang as Clang
import DW.Common (HasCallStack, liftIO, runEff, runError, runReader, withRegion)
import qualified DW.Compiler as Compiler
import DW.Config (ConfigData (..), LogLevel (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as LazyText
import Effectful.Error.Static (prettyCallStack)
import qualified DW.EmitC as EmitC
import DW.Error (displayError)
import DW.Logging (noOpLogger, runLog, scribe, standardLoggerWithIgnoredFunctions)
import qualified DW.LoopPass as LoopPass
import qualified DW.LowerPass as LowerPass
import qualified DW.Parser as Parser
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import qualified DW.Typechecker as Typechecker

run :: (HasCallStack) => ConfigData -> IO ExitCode
run cfg = do
  source <- Text.IO.readFile (Text.unpack cfg.sourceFile)

  let logger = if cfg.logLevel == Loud then standardLoggerWithIgnoredFunctions ignoredFunctions else noOpLogger

  let write = case cfg.logLevel of
        Quiet -> const $ return ()
        Default -> putStrLn
        Loud -> runEff . runLog logger . scribe . LazyText.pack

  let region msg f = case cfg.logLevel of
        Quiet -> f
        Default -> do liftIO $ putStrLn msg; f
        Loud -> withRegion (LazyText.pack msg) f

  executableName <- runEff $ runLog logger $ runError $ runReader cfg $ do
    -- Passes 1 and 2: Lexing and parsing
    ast <- region "Lexing and parsing..." do
      Parser.runParser source Parser.parseStmt

    -- Pass 3: Lowering
    loweredAST <- region "Lowering AST..." do
      return $ LowerPass.runLowerPass ast

    -- Pass 4: Typechecking
    typedAST <- region "Typechecking AST..." do
      Typechecker.runTypechecker loweredAST

    -- Pass 5: Loop validation
    region "Validating loops..." do
      LoopPass.runLoopPass typedAST

    -- Pass 6: Compile to IR
    program <- region "Compiling to IR..." do
      Compiler.runCompiler typedAST

    -- Pass 7: Generate C
    generatedC <- region "Generating C..." do
      EmitC.runEmitC program

    -- Pass 8: Compile C with clang
    executableName <- region "Compiling with clang..." do
      Clang.compileExecutable generatedC

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
    ignoredFunctions = ["markSealed", "potentiallyBox"]