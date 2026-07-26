{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import DW.Common
import DW.Config (ConfigData (..), LogLevel (..))
import DW.Frontend qualified as Frontend

import Options.Applicative
import System.Exit (exitWith)

data BuildOpts = BuildOpts
  { sourceFile :: Text,
    outputFile :: Maybe Text,
    quiet :: Bool,
    verbose :: Bool,
    showGeneratedC :: Bool
  }

newtype LSPOpts = LSPOpts {sourceFile :: Text}

data Command = Build BuildOpts | LSP LSPOpts

parseBuild :: Parser BuildOpts
parseBuild =
  BuildOpts
    <$> argument str (metavar "SOURCE_FILE" <> help "the source code to compile")
    <*> optional (option str (long "output" <> short 'o' <> help "path to compiled executable"))
    <*> switch (long "quiet" <> short 'q' <> help "produce no output")
    <*> switch (long "verbose" <> short 'v' <> help "show detailed logs")
    <*> switch (long "show-generated-c" <> help "instead of compiling, print the generated C code")

parseLSP :: Parser LSPOpts
parseLSP = LSPOpts <$> argument str (metavar "SOURCE_FILE" <> help "the source file to watch")

buildOptsToConfig :: BuildOpts -> ConfigData
buildOptsToConfig (BuildOpts {sourceFile, outputFile, quiet, verbose, showGeneratedC}) =
  ConfigData
    { sourceFile,
      outputFile,
      logLevel = if quiet then Quiet else if verbose then Loud else Default,
      showGeneratedC = showGeneratedC
    }

lspOptsToConfig :: LSPOpts -> ConfigData
lspOptsToConfig (LSPOpts {sourceFile}) = ConfigData {sourceFile, outputFile = Nothing, logLevel = Quiet, showGeneratedC = False}

parseCLI :: Parser Command
parseCLI =
  hsubparser $
    command "build" (info (Build <$> parseBuild) (progDesc "build an executable from a source file"))
      <> command "lsp" (info (LSP <$> parseLSP) (progDesc "run the LSP server"))

main :: IO ()
main = do
  cli <-
    execParser $
      info
        (parseCLI <**> helper)
        ( header "the dogwood programming language"
            <> progDesc "prototype compiler for the dogwood programming language"
        )
  exitCode <- case cli of
    Build buildOpts -> do
      let cfg = buildOptsToConfig buildOpts
      Frontend.run cfg
    LSP lspOpts -> do
      let cfg = lspOptsToConfig lspOpts
      Frontend.lsp cfg
  exitWith exitCode
