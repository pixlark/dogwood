module IR where

import Common
import Data.Hashable (Hashable (..))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified TypedAST as T

newtype BlockId = BlockId Int
  deriving (Eq, Ord)

newtype VarId = VarId Int
  deriving (Show, Eq)

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
  deriving (Eq)

data Phi = Phi {ty :: T.TypeExpr, name :: Name, operands :: [(BlockId, Name)], span :: Span}
  deriving (Eq)

data Control = Halt | Jump BlockId | JumpIf Name BlockId BlockId
  deriving (Eq)

data SSA
  = SSA T.TypeExpr Name RHS Span
  deriving (Eq)

data Block = Block {phis :: [Phi], instructions :: [SSA], control :: Control, predecessors :: [BlockId]}
  deriving (Eq)

mkBlock :: Block
mkBlock = Block [] [] Halt []

newtype Program = Program [(BlockId, Block)]
  deriving (Eq)

class ShowWithSource a where
  showWithSource :: Text -> a -> String

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

instance Show Phi where
  show Phi {ty, name, operands} = printf "%s: %s = phi %s" (show name) (show ty) (intercalate ", " $ map (\(id, name) -> printf "%s[%s]" (show id) (show name)) operands)

instance ShowWithSource Phi where
  showWithSource source phi@Phi {span} = printf "%s// %s" left' (Text.takeWhile (/= '\n') $ fst $ getLineForSpan source span)
    where
      left = show phi
      leftLen = length left
      padTo = 40
      padAmount = max 0 (padTo - leftLen)
      left' = left ++ replicate padAmount ' '

instance Show Control where
  show Halt = printf "halt"
  show (Jump id) = printf "jump %s" (show id)
  show (JumpIf name id1 id2) = printf "jump if %s to %s else %s" (show name) (show id1) (show id2)

instance Show SSA where
  show (SSA ty name rhs _) = printf "%s: %s = %s" (show name) (show ty) (show rhs)

instance ShowWithSource SSA where
  showWithSource source ssa@(SSA _ _ _ span) = printf "%s// %s" left' (Text.takeWhile (/= '\n') $ fst $ getLineForSpan source span)
    where
      left = show ssa
      leftLen = length left
      padTo = 40
      padAmount = max 0 (padTo - leftLen)
      left' = left ++ replicate padAmount ' '

instance Show Block where
  show (Block phis insts control _) = concatMap (printf "  %s\n" . show) phis ++ concatMap (printf "  %s\n" . show) insts ++ printf "  %s\n" (show control)

instance ShowWithSource Block where
  showWithSource source (Block phis insts control _) = concatMap (printf "  %s\n" . showWithSource source) phis ++ concatMap (printf "  %s\n" . showWithSource source) insts ++ printf "  %s\n" (show control)

instance Show Program where
  show (Program blocks) =
    concatMap
      ( \(id, block@(Block _ _ _ preds)) ->
          printf "%s%s:\n%s" (show id) (if null preds then "" :: String else printf "[%s]" $ intercalate ", " $ map show preds) (show block)
      )
      blocks

instance ShowWithSource Program where
  showWithSource source (Program blocks) =
    concatMap
      ( \(id, block@(Block _ _ _ preds)) ->
          printf "%s%s:\n%s" (show id) (if null preds then "" :: String else printf "[%s]" $ intercalate ", " $ map show preds) (showWithSource source block)
      )
      blocks

instance Hashable Name where
  hash (Name name) = hash name
  hashWithSalt salt (Name name) = hashWithSalt salt name

instance Hashable BlockId where
  hash (BlockId id) = hash id
  hashWithSalt salt (BlockId id) = hashWithSalt salt id

instance Hashable VarId where
  hash (VarId id) = hash id
  hashWithSalt salt (VarId id) = hashWithSalt salt id
