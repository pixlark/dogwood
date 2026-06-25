module IR where

import Common
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified TypedAST as T

newtype BlockId = BlockId Int
  deriving (Eq)

newtype Name = Name Int
  deriving (Eq)

data RHS
  = -- Literals
    RUndefined
  | RVoid
  | RInt Int
  | RBool Bool
  | -- Operators
    RBinOp T.Operator Name Name
  | RUnaryOp T.Operator Name
  | -- Control flow
    RCall Name [Name]
  | -- Phi
    RPhi (NonEmpty (BlockId, Name))
  | RPhiPlaceholder
  deriving (Eq)

data Control = Halt | Jump BlockId | JumpIf Name BlockId BlockId
  deriving (Eq)

data SSA
  = SSA T.TypeExpr Name RHS
  deriving (Eq)

data Block = Block [SSA] Control
  deriving (Eq)

mkBlock :: Block
mkBlock = Block [] Halt

newtype Program = Program [(BlockId, Block)]
  deriving (Eq)

instance Show BlockId where
  show (BlockId n) = printf "__%d" n

instance Show Name where
  show (Name n) = printf "_%d" n

instance Show RHS where
  show RUndefined = "undefined"
  show RVoid = "void"
  show (RInt n) = show n
  show (RBool b) = if b then "true" else "false"
  show (RBinOp op l r) = printf "%s %s %s" (show l) (show op) (show r)
  show (RUnaryOp op v) = printf "%s%s" (show op) (show v)
  show (RCall fn args) = printf "call %s (%s)" (show fn) (intercalate ", " $ map show args)
  show (RPhi phiPairs) = printf "phi %s" (intercalate ", " $ map (\(id, name) -> printf "%s[%s]" (show id) (show name)) $ NE.toList phiPairs)
  show RPhiPlaceholder = "phi (?)"

instance Show Control where
  show Halt = printf "halt"
  show (Jump id) = printf "jump %s" (show id)
  show (JumpIf name id1 id2) = printf "jump if %s to %s else %s" (show name) (show id1) (show id2)

instance Show SSA where
  show (SSA ty name rhs) = printf "%s: %s = %s" (show name) (show ty) (show rhs)

instance Show Block where
  show (Block insts control) = concatMap (printf "  %s\n" . show) insts ++ printf "  %s\n" (show control)

instance Show Program where
  show (Program blocks) = concatMap (\(id, block) -> printf "%s:\n%s" (show id) (show block)) blocks