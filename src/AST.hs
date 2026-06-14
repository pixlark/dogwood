{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module AST where

import Data.Text (Text)

data BuiltinType = Void | Bool | Int
  deriving (Eq, Show)

newtype NamespacedIdentifier = NamespacedIdentifier [Text]
  deriving (Eq, Show)
