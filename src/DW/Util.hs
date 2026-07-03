module DW.Util
  ( eitherFromMaybe,
    orElse,
    safeHead,
    safeLast,
    zoomState,
    (!?),
    modifyElement,
    modifyElementBy,
    getSingleElement,
    stripCallStack,
    insertAssoc,
    (<$$>),
  )
where

import Data.List (findIndex)
import Data.Maybe
import Effectful
import Effectful.Error.Static (CallStack)
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
  (State s :> es)
  => (s -> t)
  -- ^ setter
  -> (t -> s -> s)
  -- ^ getter
  -> Eff (State t : es) a
  -> Eff es a
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

getSingleElement :: [a] -> (a -> Bool) -> Maybe a
getSingleElement list predicate = case filter predicate list of
  [] -> Nothing
  [x] -> Just x
  _ -> Nothing

modifyElementBy :: [a] -> (a -> Bool) -> (a -> a) -> Maybe [a]
modifyElementBy list predicate modifier = case filter (predicate . snd) (zip [0 ..] list) of
  [] -> Nothing
  [(i, x)] -> Just $ take i list ++ [modifier x] ++ drop (i + 1) list
  _ -> Nothing

modifyElement :: (Eq a) => [a] -> a -> (a -> a) -> Maybe [a]
modifyElement list elem = modifyElementBy list (== elem)

stripCallStack :: Either (CallStack, a) b -> Either a b
stripCallStack (Left (_, x)) = Left x
stripCallStack (Right x) = Right x

insertAssoc :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
insertAssoc key value list = case idx of
  Nothing -> list ++ [(key, value)]
  Just idx -> take idx list ++ [(key, value)] ++ drop (idx + 1) list
  where
    idx = findIndex ((== key) . fst) list

(<$$>) :: (Functor f, Functor f') => (a -> b) -> f' (f a) -> f' (f b)
(<$$>) = fmap . fmap
