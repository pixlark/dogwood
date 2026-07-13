{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module DW.Compiler.Internal.Users where

import DW.Common
import DW.Compiler.Internal.Types
import DW.IR (BlockId, Phi (..), Term)
import DW.Util

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

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

-- replaceTerm :: (Term, Term) -> Term -> Term
-- replaceTerm (ifTerm, withTerm) term = if ifTerm == term then withTerm else term

-- replaceUser :: (State Compiler :> es) => UserReference -> (Term, Term) -> Eff es ()
-- replaceUser user replacement = case user of
--   (PhiUser (PhiReference {term, inBlock})) -> undefined
--   (SSAUser (InstReference {term, inBlock})) -> undefined
--   (ControlUser (ControlReference {inBlock})) -> undefined
--   where
--     replace = replaceTerm replacement

-- class DereferenceUser e u a | u -> a, u -> e where
--   dereferenceUser :: (e :> es) => u -> (a -> Eff es a) -> Eff es ()

-- class User a where
--   replaceUser :: a -> (Term, Term) -> a

-- class DereferenceUser m a | a -> m where
--   dereferenceUser :: (Monad m) => UserReference -> m (a -> m a)

-- replaceTerm :: Term -> (Term, Term) -> Term
-- replaceTerm term (ifTerm, withTerm) = if term == ifTerm then withTerm else term

-- runReplacement :: (DereferenceUser m a, User a, Monad m) => UserReference -> (Term, Term) -> m a
-- runReplacement ref repl = do
--   modifier <- dereferenceUser ref

--   _

-- a function to replace within a user
-- replaceUser :: a -> (Term, Term) -> a
-- replaceUser = undefined

-- dereferenceUser :: (State s :> es, HasUserMap s) => UserReference -> Eff es a
-- dereferenceUser = undefined

-- inplaceReplaceUser :: (State s :> es, HasUserMap s) => UserReference -> (Term, Term) -> Eff es ()
-- inplaceReplaceUser = undefined

-- data UserLens a = UserLens (a -> m a)

-- class DereferenceUser a where
--   dereferenceUser :: UserReference -> a

-- class ReplaceUser a where
--   replaceUser :: a -> (Term, Term) -> a

-- instance ReplaceUser Phi where
--   replaceUser :: Phi -> (Term, Term) -> Phi
--   replaceUser phi@Phi {operands} = undefined

-- dereference :: (ReplaceUser r) => UserReference -> r
-- dereference = undefined
