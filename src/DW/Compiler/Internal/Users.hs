{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module DW.Compiler.Internal.Users where

import DW.Common
import DW.IR (BlockId, Term)
import DW.Util

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

-- | Uniquely identifies an instruction
data UserReference
  = PhiUser Term BlockId
  | SSAUser Term BlockId
  | ControlUser BlockId
  deriving (Show, Eq)

-- | Maps `Term`s to the places that use that term
type UserMap = HashMap Term [UserReference]

class HasUserMap s where
  getUserMap :: s -> UserMap
  setUserMap :: UserMap -> s -> s

mkUserMap :: UserMap
mkUserMap = HashMap.empty

getUsers :: (State s :> es, HasUserMap s) => Term -> Eff es [UserReference]
getUsers term = do
  userMap <- gets getUserMap
  return (HashMap.lookup term userMap `orElse` [])

addUser :: (State s :> es, HasUserMap s) => Term -> UserReference -> Eff es ()
addUser term userRef = do
  userMap <- gets getUserMap
  let existing = HashMap.lookup term userMap `orElse` []
      userMap' = HashMap.insert term (existing ++ [userRef]) userMap
  modify (setUserMap userMap')

removeUser :: (State s :> es, HasUserMap s) => Term -> UserReference -> Eff es ()
removeUser term userRef = do
  userMap <- gets getUserMap
  let existing = HashMap.lookup term userMap `orElse` []
      userMap' = HashMap.insert term (filter (/= userRef) existing) userMap
  modify (setUserMap userMap')

removeAllUsers :: (State s :> es, HasUserMap s) => Term -> Eff es ()
removeAllUsers term = do
  userMap <- gets getUserMap
  let userMap' = HashMap.delete term userMap
  modify (setUserMap userMap')
