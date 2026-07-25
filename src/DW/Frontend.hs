{-# LANGUAGE TupleSections #-}

module DW.Frontend (run, lsp) where

import DW.AST (node)
import DW.Clang qualified as Clang
import DW.Common (HasCallStack, forM_, liftIO, runEff, runError, runErrorNoCallStack, runReader, traceShowId, when, withRegion)
import DW.Compiler.Internal qualified as Compiler
import DW.Config (ConfigData (..), LogLevel (..))
import DW.ConstExprPass qualified as ConstExprPass
import DW.EmitC qualified as EmitC
import DW.Error (InternalCompilerError (..), displayError)
import DW.Error.Internal.ErrorsEffect (abortIfAnyErrors, runErrorAsErrors, runErrors, runErrorsNoCallStack)
import DW.LSP qualified as LSP
import DW.Logging (Logger, noOpLogger, runLog, scribe, standardLoggerWithIgnoredFunctions)
import DW.LoopPass qualified as LoopPass
import DW.LowerPass qualified as LowerPass
import DW.NameResolutionPass qualified as NameResolutionPass
import DW.Parser qualified as Parser
import DW.Typechecker qualified as Typechecker
import DW.TypedAST

import Control.Exception (catch)
import Data.Bifunctor (Bifunctor (..))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Text.Lazy qualified as LazyText
import Effectful.Error.Static (prettyCallStack)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

run :: (HasCallStack) => ConfigData -> IO ExitCode
run cfg = catchICE $ do
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

  executableName <- runEff $ runLog logger $ runErrors $ runReader cfg $ do
    -- Passes 1 and 2: Lexing and parsing
    ast <- region "Lexing and parsing..." do
      Parser.runParser source Parser.parseTopLevel

    -- If there any any parse errors, we give up on trying to continue.
    -- This shouldn't be a big deal because they're just syntax errors, easily fixed.
    abortIfAnyErrors

    -- Pass 3: Constexpr checking
    region "Checking constant expressions..." do
      ConstExprPass.runConstExprPass ast

    -- Pass 4: Lowering
    loweredAST <- region "Lowering AST..." do
      return $ LowerPass.runLowerPass ast

    namedAST <- region "Performing name resolution..." do
      NameResolutionPass.runNameResolution loweredAST

    abortIfAnyErrors

    -- Pass 6: Typechecking
    typedAST <- region "Typechecking AST..." do
      Typechecker.runTypechecker namedAST

    -- Pass 7: Loop validation
    region "Validating loops..." do
      LoopPass.runLoopPass typedAST

    -- At this point if we've accumulated any errors, we have to give up and report them,
    -- because the later phases rely on invariants being true of the syntax trees that they're
    -- passed (so we can't just pass them bad data and hope for the best, or the user will get
    -- a bunch of internal compiler errors).
    abortIfAnyErrors

    -- Pass 8: Compile to IR
    program <- region "Compiling to IR..." do
      Compiler.runCompiler typedAST

    abortIfAnyErrors

    -- Pass 9: Generate C
    generatedC <- region "Generating C..." do
      EmitC.runEmitC program

    -- Pass 10: Compile C with clang
    executableName <- region "Compiling with clang..." do
      Clang.compileExecutable generatedC

    abortIfAnyErrors

    return executableName

  exitCode <- case executableName of
    Left errs -> do
      forM_ errs $ \(callstack, e) -> do
        when (cfg.logLevel == Loud) $ write (prettyCallStack callstack)
        write $ Text.unpack $ displayError source e
      return (ExitFailure 1)
    Right executableName -> do
      write $ "Compiled successfully into " ++ executableName
      return ExitSuccess

  return exitCode
  where
    -- functions that we want to ignore when marking the current function in the logging framework
    ignoredFunctions = ["markSealed", "potentiallyBox"]
    catchICE :: IO ExitCode -> IO ExitCode
    catchICE f =
      catch
        f
        ( \((InternalCompilerError cs) :: InternalCompilerError) -> do
            putStrLn $ prettyCallStack cs
            putStrLn ""
            putStrLn "===== INTERNAL COMPILER ERROR ====="
            putStrLn " Please leave a bug report with a"
            putStrLn " description of how this happened"
            putStrLn "==================================="
            putStrLn ""
            return (ExitFailure 1)
        )

lsp :: (HasCallStack) => ConfigData -> IO ExitCode
lsp cfg = do
  let logger = noOpLogger

  LSP.run $ runner logger
  where
    loadSource logger = runEff $ runLog logger $ runErrorNoCallStack do
      liftIO $ Text.IO.readFile (Text.unpack cfg.sourceFile)
    parseAndValidateAST logger source = runEff $ runLog logger $ runErrorsNoCallStack $ do
      -- Passes 1 and 2: Lexing and parsing
      ast <- runErrorAsErrors $ Parser.runParser source Parser.parseTopLevel

      abortIfAnyErrors

      -- Pass 3: Constexpr checking
      ConstExprPass.runConstExprPass ast

      -- Pass 4: Lowering
      let loweredAST = LowerPass.runLowerPass ast

      -- Pass 5: Name resolution
      namedAST <- NameResolutionPass.runNameResolution loweredAST

      -- Pass 6: Typechecking
      typedAST <- Typechecker.runTypechecker namedAST

      -- Pass 7: Loop validation
      LoopPass.runLoopPass typedAST

      return (source, ast, typedAST)
    runner :: Logger -> LSP.Runner
    runner logger = do
      source <- loadSource logger
      case source of
        Left (_, e) -> return $ Left (Text.empty, e)
        Right source -> do
          result <- parseAndValidateAST logger source
          return $ (source,) `first` result
