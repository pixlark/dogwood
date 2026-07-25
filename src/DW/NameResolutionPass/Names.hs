{-# LANGUAGE NoFieldSelectors #-}

module DW.NameResolutionPass.Names where

import DW.Common

import Data.Hashable (Hashable (..))
import Data.Text (unpack)

data VarName = VarName {id :: Int, text :: Text}

instance Show VarName where
  show VarName {text} = unpack text

instance Eq VarName where
  VarName {id = id1} == VarName {id = id2} | id1 /= id2 = False
  {- HLINT ignore -}
  VarName {text = t1} == VarName {text = t2} = if t1 /= t2 then throwICE else True

instance Hashable VarName where
  hash VarName {id} = hash id
  hashWithSalt salt VarName {id} = hashWithSalt salt id

getVarText :: VarName -> Text
getVarText VarName {text} = text

getVarId :: VarName -> Int
getVarId VarName {id} = id
