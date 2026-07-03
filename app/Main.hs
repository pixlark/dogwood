{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import DW.Common
import DW.Config (ConfigData (..), LogLevel (..))
import qualified DW.Frontend as Frontend
import Options.Applicative
import System.Exit (exitWith)

data CLI = CLI {sourceFile :: Text, outputFile :: Maybe Text, quiet :: Bool, verbose :: Bool}

parseCLI :: Parser CLI
parseCLI =
  CLI
    <$> argument str (metavar "SOURCE_FILE" <> help "the source code to compile")
    <*> optional (option str (long "output" <> short 'o' <> help "path to compiled executable"))
    <*> switch (long "quiet" <> short 'q' <> help "produce no output")
    <*> switch (long "verbose" <> short 'v' <> help "show detailed logs")

cliToConfig :: CLI -> ConfigData
cliToConfig (CLI {sourceFile, outputFile, quiet, verbose}) =
  ConfigData
    { sourceFile,
      outputFile,
      logLevel = if quiet then Quiet else if verbose then Loud else Default
    }

main :: IO ()
main = do
  cli <-
    execParser $
      info
        (parseCLI <**> helper)
        ( header "prototype -- WIP compiler for new language"
            <> progDesc "prototype compiler for new language"
        )
  let cfg = cliToConfig cli
  exitCode <- Frontend.run cfg
  exitWith exitCode
