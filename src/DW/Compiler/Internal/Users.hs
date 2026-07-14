module DW.Compiler.Internal.Users where

import DW.Common
import DW.Compiler.Internal.Lenses
import DW.Compiler.Internal.Types
import DW.IR
import DW.Lens
import DW.Util

import Data.Bifunctor (second)
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

replaceTermInUser :: UserReference -> (Term, Term) -> Compiler -> Compiler
replaceTermInUser ref (ifTerm, withTerm) compiler = compiler & activeFnLens %~ modify ref
  where
    -- grab the active function
    activeFnLens = programL % programIso % at compiler.activeFn % unwrapICEL
    -- grab the block with the given ID (or internal compiler error)
    blockLens id = fnsL % alist % at id % unwrapICEL

    replace term = if term == ifTerm then withTerm else term

    -- TODO: biplate?
    replaceRHS :: RHS -> RHS
    replaceRHS (RBinOp t l r) = RBinOp t (replace l) (replace r)
    replaceRHS (RUnaryOp t v) = RUnaryOp t (replace v)
    replaceRHS (RCall t ts) = RCall (replace t) (map replace ts)
    replaceRHS (RBox t v) = RBox t (replace v)
    replaceRHS rhs = rhs

    -- TODO: also maybe biplatable?
    replaceControl :: Control -> Control
    replaceControl Halt = Halt
    replaceControl (Jump b) = Jump b
    replaceControl (JumpIf v t1 t2) = JumpIf (replace v) t1 t2
    replaceControl (Ret v) = Ret v

    modify :: UserReference -> FnDef -> FnDef
    modify (PhiUser (PhiReference {term, inBlock})) fnDef =
      fnDef
        & blockLens inBlock
        % phisL
        % singleElementICEL (\(Phi {term = t}) -> term == t)
        % phiOperandsL
        %~ map (second replace)
    modify (SSAUser (InstReference {term, inBlock})) fnDef =
      fnDef
        & blockLens inBlock
        % instructionsL
        % singleElementICEL (\(SSA {term = t}) -> term == t)
        % ssaRhsL
        %~ replaceRHS
    modify (ControlUser (ControlReference {inBlock})) fnDef =
      fnDef
        & blockLens inBlock
        % controlL
        %~ replaceControl
