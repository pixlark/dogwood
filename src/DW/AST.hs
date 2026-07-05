{-# LANGUAGE TypeFamilies #-}

module DW.AST where

import DW.Common hiding (Writer, execWriter, tell)

import Control.Monad.Writer
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

class SyntaxTree t where
  node :: t a -> a
  spanOf :: t a -> Span

data AST a = AST a Span
  deriving
    (Eq)

instance SyntaxTree AST where
  node (AST x _) = x
  spanOf (AST _ s) = s

instance Functor AST where
  fmap f (AST x span) = AST (f x) span

data ValueTypeExpr = Any | Void | Bool | Int | NamespacedIdentifier [Text] | Function [AST TypeExpr] (AST TypeExpr)
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
  | BinaryOperator Operator (AST Expr) (AST Expr)
  | UnaryOperator Operator (AST Expr)
  | FunctionCall {function :: AST Expr, arguments :: [AST Expr]}
  | ExprBody Body
  | IfChain (NE.NonEmpty (AST Expr, AST Expr)) (Maybe (AST Expr))
  | Builtin Text
  deriving (Eq)

newtype LValue = LVariable T.Text
  deriving (Eq)

newtype Body = Body [AST Stmt]
  deriving (Eq)

data Stmt
  = Let {name :: AST T.Text, type_ :: Maybe (AST TypeExpr), value :: AST Expr}
  | Assign {lvalue :: AST LValue, value :: AST Expr}
  | ExprStmt {value :: AST Expr, semicolon :: Bool}
  | Return (Maybe (AST Expr))
  | Break
  | Loop (AST Body)
  deriving (Eq)

data AnyAST = AnyStmt Stmt | AnyExpr Expr | AnyLValue LValue | AnyBody Body | AnyTypeExpr TypeExpr | AnyText T.Text
  deriving (Show)

makeValueExpr :: ValueTypeExpr -> TypeExpr
makeValueExpr valueExpr = TypeExpr {reference = False, valueExpr}

makeReferenceExpr :: ValueTypeExpr -> TypeExpr
makeReferenceExpr valueExpr = TypeExpr {reference = True, valueExpr}

instance (Show a) => Show (AST a) where
  show (AST value _) = show value

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
      (AST (Variable name) _) -> tell $ T.unpack name
      _ -> tell $ printf "(%s)" (show function)
    tell "("
    forM_ (take (length arguments - 1) arguments) $ \arg -> do
      tell $ show arg
      tell ", "
    tell $ show $ last arguments
    tell ")"
  show (Variable sym) = T.unpack sym
  show (ExprBody body) = show body
  show (IfChain bodies elseBody) = execWriter $ do
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
      writeIf :: (AST Expr, AST Expr) -> Writer String ()
      writeIf (condition, body) = do
        tell "if "
        tell $ show condition
        tell " "
        tell $ show body
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
  show (Let {name = (AST name _), type_, value}) = case type_ of
    Just type_ -> printf "let %s: %s = %s;" (T.unpack name) (show type_) (show value)
    Nothing -> printf "let %s = %s;" (T.unpack name) (show value)
  show (Assign {lvalue, value}) = printf "%s = %s;" (show lvalue) (show value)
  show (ExprStmt {value, semicolon = True}) = printf "%s;" (show value)
  show (ExprStmt {value, semicolon = False}) = printf "%s" (show value)
  show (Return Nothing) = "return;"
  show (Return (Just value)) = printf "return %s;" (show value)
  show Break = "break;"
  show (Loop body) = printf "loop %s" (show body)
