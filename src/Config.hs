module Config (ConfigData (..), Config, LogLevel (..)) where

import Common (Reader, Text)

data ConfigData = ConfigData {sourceFile :: Text, outputFile :: Maybe Text, logLevel :: LogLevel}

data LogLevel = Quiet | Default | Loud
  deriving (Eq)

type Config = Reader ConfigData
