{-# LANGUAGE MultiParamTypeClasses #-}

module DW.Compiler.Internal.Types where

import DW.Error
import DW.IR
import DW.LexicalScopes
import DW.TypedAST

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable
import Data.Text.Format
import Data.Text.Lazy qualified

-------------------
------ TYPES ------
-------------------

data Compiler = Compiler
  { labelCounter :: Int,
    termCounter :: Int,
    blockCounter :: Int,
    varCounter :: Int,
    fnCounter :: Int,
    program :: Program,
    activeFn :: FnId,
    activeBlock :: BlockId,
    scopes :: LexicalScopes AbstractVariable,
    -- | Each entry in this map represents the SSA term associated with a particular AST variable
    -- | in a particular block. these get used to fill out phi functions.
    -- | Equivalent of `currentDef` in the Braun construction.
    variablesPerBlock :: HashMap (VarId, BlockId) Term,
    -- | When generating code, sometimes we reach a point where we can't be sure what `Term` refers
    -- | to a given variable. In those instances, we generate an empty phi instruction, and mark it
    -- | in this map so that we can come back to it later when that block is sealed.
    incompletePhis :: HashMap BlockId [IncompletePhi],
    -- | Each entry in this map represents an instance in the IR where a `Term` gets used, whether
    -- | that's as an operand to an instruction, as an operand to a phi instruction, or as the condition
    -- | in a `JumpIf` control.
    userMap :: UserMap,
    -- | This just keeps track of which blocks are sealed (meaning their predecessors are all known)
    sealed :: HashSet BlockId,
    -- | If we're inside a loop, this points to the basic block that follows the loop
    -- | (in other words, where we jump when we hit a break statement)
    currentBreakBlocks :: [BlockId]
  }
  deriving (Show, Eq)

-- | Refers to a unique variable in the original source code
data AbstractVariable = AbstractVariable {varId :: VarId, ty :: TypeExpr, span :: Span}
  deriving (Show, Eq)

-- | Points to a phi instruction that hasn't been filled out yet
data IncompletePhi = IncompletePhi {reference :: PhiReference, forVariable :: VarId}
  deriving (Eq)

newtype VarId = VarId Int
  deriving (Show, Eq)

data PhiReference = PhiReference {term :: Term, inBlock :: BlockId}
  deriving (Show, Eq)

data SSAReference = SSAReference {term :: Term, inBlock :: BlockId}
  deriving (Show, Eq)

data SetStaticReference = SetStaticReference {label :: Label, inBlock :: BlockId}
  deriving (Show, Eq)

newtype ControlReference = ControlReference {inBlock :: BlockId}
  deriving (Show, Eq)

data UserReference
  = PhiUser PhiReference
  | SSAUser SSAReference
  | SetStaticUser SetStaticReference
  | ControlUser ControlReference
  deriving (Show, Eq)

-- | Maps `Term`s to the places that use that term
type UserMap = HashMap Term [UserReference]

---------------------
------ CLASSES ------
---------------------

class HasUserMap s where
  getUserMap :: s -> UserMap
  setUserMap :: UserMap -> s -> s

-----------------------
------ INSTANCES ------
-----------------------

instance Hashable VarId where
  hash (VarId id) = hash id
  hashWithSalt salt (VarId id) = hashWithSalt salt id

instance Show IncompletePhi where
  show (IncompletePhi {reference = PhiReference term inBlock, forVariable}) =
    Data.Text.Lazy.unpack $ format "φ({} for {} in {})" (Shown term, Shown forVariable, Shown inBlock)

instance HasUserMap Compiler where
  getUserMap = userMap
  setUserMap userMap c = c {userMap}

instance HasLexicalScopes AbstractVariable Compiler where
  getScopes = scopes
  setScopes scopes c = c {scopes}
