{-# LANGUAGE TypeFamilies #-}

module DW.LoweredAST where

import DW.AST (SyntaxTree (..))
import DW.Common hiding (Writer, execWriter, tell)
import Control.Monad.Writer
import qualified Data.Text as T

data LST a = LST a Span
  deriving
    (Eq)

instance SyntaxTree LST where
  node (LST x _) = x
  spanOf (LST _ s) = s

instance Functor LST where
  fmap f (LST x span) = LST (f x) span

data ValueTypeExpr = Any | Void | Bool | Int | NamespacedIdentifier [Text] | Function [LST TypeExpr] (LST TypeExpr)
  deriving (Eq)

data TypeExpr = TypeExpr {reference :: Bool, valueExpr :: ValueTypeExpr}
  deriving (Eq)

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
  | Modulo
  deriving (Eq)

data Expr
  = UndefinedLit
  | VoidLit
  | BoolLit Bool
  | IntLit Int
  | Variable Text
  | BinaryOperator Operator (LST Expr) (LST Expr)
  | UnaryOperator Operator (LST Expr)
  | FunctionCall {function :: LST Expr, arguments :: [LST Expr]}
  | ExprBody Body
  | -- | IfChain (NE.NonEmpty (LST Expr, LST Expr)) (Maybe (LST Expr))
    IfThen (LST Expr) (LST Expr) (LST Expr)
  | Builtin Text
  deriving (Eq)

newtype LValue = LVariable T.Text
  deriving (Eq)

newtype Body = Body [LST Stmt]
  deriving (Eq)

data Stmt
  = Let {name :: LST T.Text, type_ :: LST TypeExpr, value :: LST Expr}
  | Assign {lvalue :: LST LValue, value :: LST Expr}
  | ExprStmt {value :: LST Expr, semicolon :: Bool}
  | Return (Maybe (LST Expr))
  | Break
  | Loop (LST Body)
  deriving (Eq)

makeValueExpr :: ValueTypeExpr -> TypeExpr
makeValueExpr valueExpr = TypeExpr {reference = False, valueExpr}

makeReferenceExpr :: ValueTypeExpr -> TypeExpr
makeReferenceExpr valueExpr = TypeExpr {reference = True, valueExpr}

instance (Show a) => Show (LST a) where
  show (LST value _) = show value

instance Show ValueTypeExpr where
  show Any = "any"
  show Void = "void"
  show Bool = "bool"
  show Int = "int"
  show (NamespacedIdentifier parts) = T.unpack $ T.intercalate "::" parts
  show (Function params ret) = execWriter do
    tell "fn("
    tell $ T.unpack $ T.intercalate ", " $ map T.show params
    tell ") -> "
    tell $ show ret

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
  show Modulo = "%"

instance Show Expr where
  show UndefinedLit = "undefined"
  show VoidLit = "void"
  show (BoolLit b) = if b then "true" else "false"
  show (IntLit n) = show n
  show (BinaryOperator op a b) = printf "(%s %s %s)" (show a) (show op) (show b)
  show (UnaryOperator op e) = printf "(%s%s)" (show op) (show e)
  show (FunctionCall {function, arguments}) = execWriter $ do
    case function of
      (LST (Variable name) _) -> tell $ T.unpack name
      _ -> tell $ printf "(%s)" (show function)
    tell "("
    forM_ (take (length arguments - 1) arguments) $ \arg -> do
      tell $ show arg
      tell ", "
    tell $ show $ last arguments
    tell ")"
  show (Variable sym) = T.unpack sym
  show (ExprBody body) = show body
  show (IfThen condition body elseBody) = execWriter $ do
    tell "if "
    tell $ show condition
    tell " "
    tell $ show body
    tell " else "
    tell $ show elseBody
  show (Builtin name) = printf "builtin %s" name

instance Show LValue where
  show (LVariable name) = T.unpack name

instance Show Body where
  show (Body stmts) = execWriter $ do
    tell "{\n"
    forM_ stmts $ \stmt -> do
      tell (show stmt)
      tell "\n"
    tell "}"

instance Show Stmt where
  show (Let {name = (LST name _), type_, value}) = printf "let %s: %s = %s;" (T.unpack name) (show type_) (show value)
  show (Assign {lvalue, value}) = printf "%s = %s;" (show lvalue) (show value)
  show (ExprStmt {value, semicolon = True}) = printf "%s;" (show value)
  show (ExprStmt {value, semicolon = False}) = printf "%s" (show value)
  show (Return Nothing) = "return;"
  show (Return (Just value)) = printf "return %s;" (show value)
  show Break = "break;"
  show (Loop body) = printf "loop %s" (show body)
