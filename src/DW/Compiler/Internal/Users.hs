{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module DW.Compiler.Internal.Users where

import DW.Common
import DW.IR (BlockId, Name)
import DW.Util

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

-- | Uniquely identifies an instruction
data UserReference
  = PhiUser Name BlockId
  | SSAUser Name BlockId
  | ControlUser BlockId
  deriving (Show, Eq)

-- | Maps `Name`s to the places that use that name
type UserMap = HashMap Name [UserReference]

class HasUserMap s where
  getUserMap :: s -> UserMap
  setUserMap :: UserMap -> s -> s

mkUserMap :: UserMap
mkUserMap = HashMap.empty

getUsers :: (State s :> es, HasUserMap s) => Name -> Eff es [UserReference]
getUsers name = do
  userMap <- gets getUserMap
  return (HashMap.lookup name userMap `orElse` [])

addUser :: (State s :> es, HasUserMap s) => Name -> UserReference -> Eff es ()
addUser name userRef = do
  userMap <- gets getUserMap
  let existing = HashMap.lookup name userMap `orElse` []
      userMap' = HashMap.insert name (existing ++ [userRef]) userMap
  modify (setUserMap userMap')

removeUser :: (State s :> es, HasUserMap s) => Name -> UserReference -> Eff es ()
removeUser name userRef = do
  userMap <- gets getUserMap
  let existing = HashMap.lookup name userMap `orElse` []
      userMap' = HashMap.insert name (filter (/= userRef) existing) userMap
  modify (setUserMap userMap')

removeAllUsers :: (State s :> es, HasUserMap s) => Name -> Eff es ()
removeAllUsers name = do
  userMap <- gets getUserMap
  let userMap' = HashMap.delete name userMap
  modify (setUserMap userMap')
