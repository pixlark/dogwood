{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- #define USE_DERIVED_SHOW

module AST where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Lexer (Span)
import Text.Printf

data AST a = AST a Span
  deriving (Eq
#ifdef USE_DERIVED_SHOW
  , Show
#endif
  )

instance Functor AST where
  fmap f (AST x span) = AST (f x) span

data ValueTypeExpr = Void | Bool | Int | NamespacedIdentifier [Text]
  deriving (Eq
#ifdef USE_DERIVED_SHOW
  , Show
#endif
  )

data TypeExpr = TypeExpr {reference :: Bool, valueExpr :: ValueTypeExpr}
  deriving (Eq
#ifdef USE_DERIVED_SHOW
  , Show
#endif
  )

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
  deriving (Eq
#ifdef USE_DERIVED_SHOW
  , Show
#endif
  )

data Expr
  = VoidLit
  | BoolLit Bool
  | IntLit Int
  | BinaryOperator Operator (AST Expr) (AST Expr)
  | UnaryOperator Operator (AST Expr)
  deriving (Eq
#ifdef USE_DERIVED_SHOW
  , Show
#endif
  )

newtype LValue = LVariable T.Text
  deriving (Eq
#ifdef USE_DERIVED_SHOW
  , Show
#endif
  )

data Body = Body [AST Stmt] (Maybe (AST Expr))
  deriving (Eq
#ifdef USE_DERIVED_SHOW
  , Show
#endif
  )

data Stmt
  = Let {name :: AST T.Text, type_ :: AST TypeExpr, value :: AST Expr}
  | Assign {lvalue :: AST LValue, value :: AST Expr}
  | ExprStmt (AST Expr)
  | Return (Maybe (AST Expr))
  | Break
  | StmtBody (AST Body)
  deriving (Eq
#ifdef USE_DERIVED_SHOW
  , Show
#endif
  )

makeValueExpr :: ValueTypeExpr -> TypeExpr
makeValueExpr valueExpr = TypeExpr {reference = False, valueExpr}

makeReferenceExpr :: ValueTypeExpr -> TypeExpr
makeReferenceExpr valueExpr = TypeExpr {reference = True, valueExpr}

#ifndef USE_DERIVED_SHOW
instance (Show a) => Show (AST a) where
  show (AST value _) = show value

instance Show ValueTypeExpr where
  show Void = "void"
  show Bool = "bool"
  show Int = "int"
  show (NamespacedIdentifier parts) = T.unpack $ T.intercalate "::" parts

instance Show TypeExpr where
  show (TypeExpr {reference, valueExpr}) = (if reference then "&" else "") ++ show valueExpr

instance Show Operator where
  show Or = "||"
  show And = "&&"
  show Equal = "=="
  show NotEqual = "!="
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="
  show Plus = "+"
  show Minus = "-"
  show Multiply = "*"
  show Divide = "/"
  show Not = "!"

instance Show Expr where
  show VoidLit = "void"
  show (BoolLit b) = if b then "true" else "false"
  show (IntLit n) = show n
  show (BinaryOperator op a b) = printf "(%s %s %s)" (show a) (show op) (show b)
  show (UnaryOperator op e) = printf "(%s%s)" (show op) (show e)

instance Show LValue where
  show (LVariable name) = T.unpack name

instance Show Body where
  show (Body stmts finalExpr) =
    printf
      "{\n%s;%s\n}"
      (intercalate ";\n" $ show <$> stmts)
      $ case finalExpr of
        Nothing -> []
        Just expr -> "\n" ++ show expr

instance Show Stmt where
  show (Let {name = (AST name _), type_, value}) = printf "let %s: %s = %s" (T.unpack name) (show type_) (show value)
  show (Assign {lvalue, value}) = printf "%s = %s" (show lvalue) (show value)
  show (ExprStmt expr) = printf "%s" (show expr)
  show (Return Nothing) = "return"
  show (Return (Just value)) = printf "return %s" (show value)
  show Break = "break"
  show (StmtBody body) = show body
#endif