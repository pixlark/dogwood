module Util (eitherFromMaybe, orElse, safeHead, safeLast, zoomState, (!?)) where

import Data.Maybe
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local

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

-- | Allows you to "shift" a State effect so that
zoomState ::
  (State s :> es) =>
  -- | setter
  (s -> t) ->
  -- | getter
  (t -> s -> s) ->
  Eff (State t : es) a ->
  Eff es a
zoomState getter setter m = do
  st <- get
  (a, st') <- runState (getter st) m
  modify (setter st')
  return a

(!?) :: [a] -> Int -> Maybe a
{-# INLINEABLE (!?) #-}
xs !? n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n