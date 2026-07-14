{-# LANGUAGE TypeFamilies #-}

module DW.Lens
  ( module Optics,
    lensNamer,
    lensRules,
    makeLenses,
    alist,
    unwrapICEL,
    singleElementICEL,
    makeLensesWithPrefix,
  ) where

import DW.Error
import DW.Util

import Data.Char (toUpper)
import Data.Function
import Optics hiding (lensRules, makeLenses)
import Optics qualified

lensNamer = mappingNamer (\s -> [s ++ "L"])
lensRules = Optics.lensRules & lensField .~ lensNamer
makeLenses = makeLensesWith lensRules

prefixLensNamer prefix = mappingNamer (\s -> [prefix ++ (s & ix 0 %~ toUpper) ++ "L"])
prefixLensRules prefix = Optics.lensRules & lensField .~ prefixLensNamer prefix
makeLensesWithPrefix prefix = makeLensesWith (prefixLensRules prefix)

-- | Allows you to unwrap a `Maybe` in an optics path, throwing an
-- | internal compiler error if it's `Nothing`.
unwrapICEL :: Iso' (Maybe a) a
unwrapICEL = iso unwrapICE Just

-- | Modify a single element of a list as determined by a predicate.
-- | If no element exists, throw an internal compiler error.
singleElementICEL :: (a -> Bool) -> Lens' [a] a
singleElementICEL predicate = lens getter setter
  where
    getter = unwrapICE . (`getSingleElement` predicate)
    setter list x = unwrapICE $ modifyElementBy list predicate (const x)

newtype AList k v = AList [(k, v)]

alist :: Iso' [(k, v)] (AList k v)
alist = coerced

instance (Eq k) => Ixed (AList k v)

type instance Index (AList k v) = k
type instance IxValue (AList k v) = v

instance (Eq k) => At (AList k v) where
  at :: k -> Lens' (AList k v) (Maybe (IxValue (AList k v)))
  at i = lens getter setter
    where
      getter :: AList k v -> Maybe (IxValue (AList k v))
      getter (AList list) = lookup i list
      setter :: AList k v -> Maybe v -> AList k v
      setter (AList list) (Just v) = AList $ insertAssoc i v list
      setter list Nothing = list
