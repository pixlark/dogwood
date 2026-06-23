{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Util (eitherFromMaybe, orElse, safeHead, safeLast) where

import Data.Maybe

eitherFromMaybe :: e -> Maybe a -> Either e a
eitherFromMaybe _ (Just x) = Right x
eitherFromMaybe e Nothing = Left e

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ : xs) = safeLast xs
