module DW.Config (ConfigData (..), Config, LogLevel (..)) where

import DW.Common (Reader, Text)

data ConfigData = ConfigData
  { sourceFile :: Text,
    outputFile :: Maybe Text,
    logLevel :: LogLevel,
    showGeneratedC :: Bool
  }

data LogLevel = Quiet | Default | Loud
  deriving (Eq)

type Config = Reader ConfigData
