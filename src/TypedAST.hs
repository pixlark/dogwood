{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TypedAST where

import AST (SyntaxTree (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Error

data TST a = TST a Span
  deriving (Eq, Show)

instance Functor TST where
  fmap f (TST x span) = TST (f x) span

instance SyntaxTree TST where
  node (TST x _) = x
  spanOf (TST _ s) = s

data ValueTypeExpr = Void | Bool | Int | NamespacedIdentifier [Text]
  deriving (Eq, Show)

data TypeExpr = TypeExpr {reference :: Bool, valueExpr :: ValueTypeExpr}
  deriving (Eq, Show)

data Operator
  = Or
  | And
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Plus
  | Minus
  | Multiply
  | Divide
  | Not
  deriving (Eq, Show)

data Expr
  = VoidLit
  | BoolLit Bool
  | IntLit Int
  | Variable TypeExpr Text
  | BinaryOperator TypeExpr Operator (TST Expr) (TST Expr)
  | UnaryOperator TypeExpr Operator (TST Expr)
  | FunctionCall {type_ :: TypeExpr, function :: TST Expr, arguments :: [TST Expr]}
  | ExprBody TypeExpr Body
  | IfChain TypeExpr (NE.NonEmpty (TST Expr, TST Expr)) (Maybe (TST Expr))
  deriving (Eq, Show)

makeValueExpr :: ValueTypeExpr -> TypeExpr
makeValueExpr valueExpr = TypeExpr {reference = False, valueExpr}

makeReferenceExpr :: ValueTypeExpr -> TypeExpr
makeReferenceExpr valueExpr = TypeExpr {reference = True, valueExpr}

typeOf :: Expr -> TypeExpr
typeOf VoidLit = makeValueExpr Void
typeOf (BoolLit _) = makeValueExpr Bool
typeOf (IntLit _) = makeValueExpr Int
typeOf (Variable t _) = t
typeOf (BinaryOperator t _ _ _) = t
typeOf (UnaryOperator t _ _) = t
typeOf (FunctionCall {type_, function, arguments}) = type_
typeOf (ExprBody t _) = t
typeOf (IfChain t _ _) = t

data LValue = LVariable TypeExpr T.Text
  deriving (Eq, Show)

newtype Body = Body [TST Stmt]
  deriving (Eq, Show)

data Stmt
  = Let {name :: TST T.Text, type_ :: TST TypeExpr, value :: TST Expr}
  | Assign {lvalue :: TST LValue, value :: TST Expr}
  | ExprStmt {value :: TST Expr, semicolon :: Bool}
  | Return (Maybe (TST Expr))
  | Break
  | Loop (TST Body)
  deriving (Eq, Show)
