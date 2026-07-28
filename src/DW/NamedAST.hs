module DW.NamedAST where

import DW.AST (SyntaxTree (..))
import DW.Common hiding (Writer, execWriter, tell)
import DW.NameResolutionPass.Names

import Control.Monad.Writer
import Data.List (intersperse)
import Data.Text qualified as T

data NST a = NST a Span
  deriving
    (Eq)

instance SyntaxTree NST where
  node (NST x _) = x
  spanOf (NST _ s) = s

instance Functor NST where
  fmap f (NST x span) = NST (f x) span

data ValueTypeExpr = Any | Void | Bool | Int | Function [NST TypeExpr] (NST TypeExpr)
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
  = VoidLit
  | BoolLit Bool
  | IntLit Int
  | Variable VarName
  | BinaryOperator Operator (NST Expr) (NST Expr)
  | UnaryOperator Operator (NST Expr)
  | FunctionCall {function :: NST Expr, arguments :: [NST Expr]}
  | ExprBody Body
  | IfThen (NST Expr) (NST Expr) (NST Expr)
  | Builtin Text
  | Lambda {params :: [(NST TypeExpr, NST VarName)], returnType :: NST TypeExpr, body :: NST Expr}
  | NewOperator {ty :: NST TypeExpr, arguments :: [NST Expr]}
  | Dereference (NST Expr)
  deriving (Eq)

newtype LValue = LVariable VarName
  deriving (Eq)

newtype Body = Body [NST Stmt]
  deriving (Eq)

data Stmt
  = Let {name :: NST VarName, type_ :: Maybe (NST TypeExpr), value :: NST Expr}
  | Assign {lvalue :: NST LValue, value :: NST Expr}
  | ExprStmt {value :: NST Expr, semicolon :: Bool}
  | Return (Maybe (NST Expr))
  | Break
  | Loop (NST Body)
  deriving (Eq)

data TopLevelStmt = TLet {name :: NST VarName, ty :: Maybe (NST TypeExpr), value :: NST Expr}

newtype TopLevel = TopLevel [NST TopLevelStmt]

makeValueExpr :: ValueTypeExpr -> TypeExpr
makeValueExpr valueExpr = TypeExpr {reference = False, valueExpr}

makeReferenceExpr :: ValueTypeExpr -> TypeExpr
makeReferenceExpr valueExpr = TypeExpr {reference = True, valueExpr}

mkAny = makeValueExpr Any

mkVoid = makeValueExpr Void

mkBool = makeValueExpr Bool

mkInt = makeValueExpr Int

instance (Show a) => Show (NST a) where
  show (NST value _) = show value

instance Show ValueTypeExpr where
  show Any = "any"
  show Void = "void"
  show Bool = "bool"
  show Int = "int"
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
  show VoidLit = "void"
  show (BoolLit b) = if b then "true" else "false"
  show (IntLit n) = show n
  show (BinaryOperator op a b) = printf "(%s %s %s)" (show a) (show op) (show b)
  show (UnaryOperator op e) = printf "(%s%s)" (show op) (show e)
  show (FunctionCall {function, arguments}) = execWriter $ do
    case function of
      (NST (Variable name) _) -> tell $ show name
      _ -> tell $ printf "(%s)" (show function)
    tell "("
    forM_ (take (length arguments - 1) arguments) $ \arg -> do
      tell $ show arg
      tell ", "
    tell $ show $ last arguments
    tell ")"
  show (Variable sym) = show sym
  show (ExprBody body) = show body
  show (IfThen condition body elseBody) = execWriter $ do
    tell "if "
    tell $ show condition
    tell " "
    tell $ show body
    tell " else "
    tell $ show elseBody
  show (Builtin name) = printf "builtin %s" name
  show (Lambda {params, returnType, body}) = execWriter $ do
    tell "fn("
    forM_ (intersperse Nothing $ Just <$> params) $ \m -> do
      case m of
        Nothing -> tell ", "
        Just (ty, name) -> do
          tell $ show $ node name
          tell ": "
          tell $ show ty
    tell ") -> "
    tell $ show returnType
    case node body of
      ExprBody _ -> tell " "
      _ -> tell ": "
    tell $ show body
  show (NewOperator ty args) = execWriter $ do
    tell "new "
    tell $ show ty
    unless (null args) do
      tell "("
      forM_ (intersperse Nothing $ Just <$> args) $ \arg -> do
        case arg of
          Just arg -> tell $ show arg
          Nothing -> tell ", "
      tell ")"
  show (Dereference expr) = printf "*%s" (show expr)

instance Show LValue where
  show (LVariable name) = show name

instance Show Body where
  show (Body stmts) = execWriter $ do
    tell "{\n"
    forM_ stmts $ \stmt -> do
      tell (show stmt)
      tell "\n"
    tell "}"

instance Show Stmt where
  show (Let {name = (NST name _), type_, value}) = case type_ of
    Just type_ -> printf "let %s: %s = %s;" (show name) (show type_) (show value)
    Nothing -> printf "let %s = %s;" (show name) (show value)
  show (Assign {lvalue, value}) = printf "%s = %s;" (show lvalue) (show value)
  show (ExprStmt {value, semicolon = True}) = printf "%s;" (show value)
  show (ExprStmt {value, semicolon = False}) = printf "%s" (show value)
  show (Return Nothing) = "return;"
  show (Return (Just value)) = printf "return %s;" (show value)
  show Break = "break;"
  show (Loop body) = printf "loop %s" (show body)

instance Show TopLevelStmt where
  show (TLet {name = (NST name _), ty, value}) = case ty of
    Just ty -> printf "let %s: %s = %s;" (show name) (show ty) (show value)
    Nothing -> printf "let %s = %s;" (show name) (show value)

instance Show TopLevel where
  show (TopLevel stmts) = execWriter $ do
    forM_ stmts $ \stmt -> do
      tell $ show stmt
      tell "\n"
