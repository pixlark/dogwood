{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Util (eitherFromMaybe) where

eitherFromMaybe :: e -> Maybe a -> Either e a
eitherFromMaybe _ (Just x) = Right x
eitherFromMaybe e Nothing = Left e
