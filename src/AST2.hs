{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AST2 where

import Control.Monad
import Control.Monad.Writer
import Data.Functor
import Data.Kind (Constraint, Type)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Error
import Text.Printf

data ValueTypeExpr a
  = Void (VoidX a)
  | Bool (BoolX a)
  | Int (IntX a)
  | NamespacedIdentifier (NamespacedIdentifierX a) [Text]

type family VoidX a

type family BoolX a

type family IntX a

type family NamespacedIdentifierX a

data TypeExpr a = TypeExpr {typeExprX :: TypeExprX a, reference :: Bool, valueExpr :: ValueTypeExpr a}

type family TypeExprX a

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
  deriving (Eq)

data Expr a
  = VoidLit (VoidLitX a)
  | BoolLit (BoolLitX a) Bool
  | IntLit (IntLitX a) Int
  | Variable (VariableX a) Text
  | BinaryOperator (BinaryOperatorX a) Operator (Expr a) (Expr a)
  | UnaryOperator (UnaryOperatorX a) Operator (Expr a)
  | FunctionCall {functionCallX :: FunctionCallX a, function :: Expr a, arguments :: [Expr a]}
  | ExprBody (ExprBodyX a) (Body a)
  | IfChain (IfChainX a) (NE.NonEmpty (Expr a, Expr a)) (Maybe (Expr a))

type family VoidLitX a

type family BoolLitX a

type family IntLitX a

type family VariableX a

type family BinaryOperatorX a

type family UnaryOperatorX a

type family FunctionCallX a

type family ExprBodyX a

type family IfChainX a

data LValue a = LVariable (LVariableX a) T.Text

type family LVariableX a

data Body a = Body (BodyX a) [Stmt a]

type family BodyX a

data Stmt a
  = Let {letX :: LetX a, name :: T.Text, type_ :: TypeExpr a, value :: Expr a}
  | Assign {assignX :: AssignX a, lvalue :: LValue a, value :: Expr a}
  | ExprStmt {exprStmtX :: ExprStmtX a, value :: Expr a, semicolon :: Bool}
  | Return (ReturnX a) (Maybe (Expr a))
  | Break (BreakX a)
  | Loop (LoopX a) (Body a)

type family LetX a

type family AssignX a

type family ExprStmtX a

type family ReturnX a

type family BreakX a

type family LoopX a

-------------------------------------------------

type ForAST (phi :: Type -> Constraint) a =
  ( phi (VoidX a),
    phi (BoolX a),
    phi (IntX a),
    phi (NamespacedIdentifierX a),
    phi (TypeExprX a),
    phi (VoidLitX a),
    phi (BoolLitX a),
    phi (IntLitX a),
    phi (VariableX a),
    phi (BinaryOperatorX a),
    phi (UnaryOperatorX a),
    phi (FunctionCallX a),
    phi (ExprBodyX a),
    phi (IfChainX a),
    phi (LVariableX a),
    phi (BodyX a),
    phi (LetX a),
    phi (AssignX a),
    phi (ExprStmtX a),
    phi (ReturnX a),
    phi (BreakX a),
    phi (LoopX a)
  )

-- Show

-- deriving instance (ForAST Show a) => Show (ValueTypeExpr a)

-- deriving instance (ForAST Show a) => Show (TypeExpr a)

-- deriving instance (ForAST Show a) => Show (Expr a)

-- deriving instance (ForAST Show a) => Show (LValue a)

-- deriving instance (ForAST Show a) => Show (Body a)

-- deriving instance (ForAST Show a) => Show (Stmt a)

-- Eq

deriving instance (ForAST Eq a) => Eq (ValueTypeExpr a)

deriving instance (ForAST Eq a) => Eq (TypeExpr a)

deriving instance (ForAST Eq a) => Eq (Expr a)

deriving instance (ForAST Eq a) => Eq (LValue a)

deriving instance (ForAST Eq a) => Eq (Body a)

deriving instance (ForAST Eq a) => Eq (Stmt a)

-------------------------------------------------

data Parse

-- ValueTypeExpr

type instance VoidX Parse = ()

type instance BoolX Parse = ()

type instance IntX Parse = ()

type instance NamespacedIdentifierX Parse = ()

-- TypeExpr

type instance TypeExprX Parse = Span

-- Expr

type instance VoidLitX Parse = Span

type instance BoolLitX Parse = Span

type instance IntLitX Parse = Span

type instance VariableX Parse = Span

type instance BinaryOperatorX Parse = Span

type instance UnaryOperatorX Parse = Span

type instance FunctionCallX Parse = Span

type instance ExprBodyX Parse = Span

type instance IfChainX Parse = Span

-- LValue

type instance LVariableX Parse = Span

-- Body

type instance BodyX Parse = Span

-- Stmt

type instance LetX Parse = Span

type instance AssignX Parse = Span

type instance ExprStmtX Parse = Span

type instance ReturnX Parse = Span

type instance BreakX Parse = Span

type instance LoopX Parse = Span

-------------------------------------------------

makeValueExpr :: ValueTypeExpr Parse -> TypeExpr Parse
makeValueExpr valueExpr = TypeExpr {typeExprX = Span 0 0, reference = False, valueExpr}

makeReferenceExpr :: ValueTypeExpr Parse -> TypeExpr Parse
makeReferenceExpr valueExpr = TypeExpr {typeExprX = Span 0 0, reference = True, valueExpr}

instance Show (ValueTypeExpr a) where
  show (Void _) = "void"
  show (Bool _) = "bool"
  show (Int _) = "int"
  show (NamespacedIdentifier _ parts) = T.unpack $ T.intercalate "::" parts

instance Show (TypeExpr a) where
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

instance Show (Expr a) where
  show (VoidLit _) = "void"
  show (BoolLit _ b) = if b then "true" else "false"
  show (IntLit _ n) = show n
  show (BinaryOperator _ op a b) = printf "(%s %s %s)" (show a) (show op) (show b)
  show (UnaryOperator _ op e) = printf "(%s%s)" (show op) (show e)
  show (FunctionCall {function, arguments}) = execWriter $ do
    case function of
      (Variable _ name) -> tell $ T.unpack name
      _ -> tell $ printf "(%s)" (show function)
    tell "("
    forM_ (take (length arguments - 1) arguments) $ \arg -> do
      tell $ show arg
      tell ", "
    tell $ show $ last arguments
    tell ")"
  show (Variable _ sym) = T.unpack sym
  show (ExprBody _ body) = show body
  show (IfChain _ bodies elseBody) = execWriter $ do
    let first = NE.head bodies
    writeIf first
    let rest = NE.tail bodies
    forM_ rest $ \body -> do
      tell " else "
      writeIf body
    forM_ elseBody $ \body -> do
      tell " else "
      tell $ show body
    where
      writeIf :: (Expr a, Expr a) -> Writer String ()
      writeIf (condition, body) = do
        tell "if "
        tell $ show condition
        tell " "
        tell $ show body

instance Show (LValue a) where
  show (LVariable _ name) = T.unpack name

instance Show (Body a) where
  show (Body _ stmts) = execWriter $ do
    tell "{\n"
    forM_ stmts $ \stmt -> do
      tell (show stmt)
      tell "\n"
    tell "}"

instance Show (Stmt a) where
  show (Let {name, type_, value}) = printf "let %s: %s = %s;" (T.unpack name) (show type_) (show value)
  show (Assign {lvalue, value}) = printf "%s = %s;" (show lvalue) (show value)
  show (ExprStmt {value, semicolon = True}) = printf "%s;" (show value)
  show (ExprStmt {value, semicolon = False}) = printf "%s" (show value)
  show (Return _ Nothing) = "return;"
  show (Return _ (Just value)) = printf "return %s;" (show value)
  show (Break _) = "break;"
  show (Loop _ body) = printf "loop %s" (show body)
