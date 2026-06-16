{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Util (eitherFromMaybe, orElse) where

import Data.Maybe

eitherFromMaybe :: e -> Maybe a -> Either e a
eitherFromMaybe _ (Just x) = Right x
eitherFromMaybe e Nothing = Left e

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe
