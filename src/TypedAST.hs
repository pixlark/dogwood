{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TypedAST where

import AST (SyntaxTree (..))
import Control.Monad (forM_)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Error
import Text.Printf (printf)

data TST a = TST a Span
  deriving (Eq)

instance Functor TST where
  fmap f (TST x span) = TST (f x) span

instance SyntaxTree TST where
  node (TST x _) = x
  spanOf (TST _ s) = s

data ValueTypeExpr = Any | Void | Bool | Int | NamespacedIdentifier [Text] | Function [TST TypeExpr] (TST TypeExpr)
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
  deriving (Eq)

data Expr
  = UndefinedLit
  | VoidLit
  | BoolLit Bool
  | IntLit Int
  | Variable TypeExpr Text
  | BinaryOperator TypeExpr Operator (TST Expr) (TST Expr)
  | UnaryOperator TypeExpr Operator (TST Expr)
  | FunctionCall {type_ :: TypeExpr, function :: TST Expr, arguments :: [TST Expr]}
  | ExprBody Body
  | IfChain TypeExpr (NE.NonEmpty (TST Expr, TST Expr)) (Maybe (TST Expr))
  deriving (Eq)

makeValueExpr :: ValueTypeExpr -> TypeExpr
makeValueExpr valueExpr = TypeExpr {reference = False, valueExpr}

makeReferenceExpr :: ValueTypeExpr -> TypeExpr
makeReferenceExpr valueExpr = TypeExpr {reference = True, valueExpr}

typeOf :: Expr -> TypeExpr
typeOf UndefinedLit = makeValueExpr Any
typeOf VoidLit = makeValueExpr Void
typeOf (BoolLit _) = makeValueExpr Bool
typeOf (IntLit _) = makeValueExpr Int
typeOf (Variable t _) = t
typeOf (BinaryOperator t _ _ _) = t
typeOf (UnaryOperator t _ _) = t
typeOf (FunctionCall {type_, function, arguments}) = type_
typeOf (ExprBody (Body t _)) = t
typeOf (IfChain t _ _) = t

data LValue = LVariable TypeExpr T.Text
  deriving (Eq)

data Body = Body TypeExpr [TST Stmt]
  deriving (Eq)

data Stmt
  = Let {name :: TST T.Text, type_ :: TST TypeExpr, value :: TST Expr}
  | Assign {lvalue :: TST LValue, value :: TST Expr}
  | ExprStmt {value :: TST Expr, semicolon :: Bool}
  | Return (Maybe (TST Expr))
  | Break
  | Loop (TST Body)
  deriving (Eq)

instance (Show a) => Show (TST a) where
  show (TST value _) = show value

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

instance Show Expr where
  show UndefinedLit = "undefined"
  show VoidLit = "void"
  show (BoolLit b) = if b then "true" else "false"
  show (IntLit n) = show n
  show (BinaryOperator t op a b) = printf "(%s %s %s : %s)" (show a) (show op) (show b) (show t)
  show (UnaryOperator t op e) = printf "(%s%s : %s)" (show op) (show e) (show t)
  show (FunctionCall {type_, function, arguments}) = execWriter $ do
    tell "("
    case function of
      (TST (Variable _ name) _) -> tell $ T.unpack name
      _ -> tell $ printf "(%s)" (show function)
    tell "("
    forM_ (take (length arguments - 1) arguments) $ \arg -> do
      tell $ show arg
      tell ", "
    tell $ show $ last arguments
    tell $ printf ") : %s)" (show type_)
  show (Variable t sym) = printf "(%s : %s)" sym (show t)
  show (ExprBody body) = show body
  show (IfChain t bodies elseBody) = execWriter $ do
    tell "("
    let first = NE.head bodies
    writeIf first
    let rest = NE.tail bodies
    forM_ rest $ \body -> do
      tell " else "
      writeIf body
    forM_ elseBody $ \body -> do
      tell " else "
      tell $ show body
    tell $ printf ") : %s" (show t)
    where
      writeIf :: (TST Expr, TST Expr) -> Writer String ()
      writeIf (condition, body) = do
        tell "if "
        tell $ show condition
        tell " "
        tell $ show body

instance Show LValue where
  show (LVariable _ name) = T.unpack name

instance Show Body where
  show (Body t stmts) = execWriter $ do
    tell "{\n"
    forM_ stmts $ \stmt -> do
      tell (show stmt)
      tell "\n"
    tell $ printf "} : %s" (show t)

instance Show Stmt where
  show (Let {name = (TST name _), type_, value}) = printf "let %s: %s = %s;" (T.unpack name) (show type_) (show value)
  show (Assign {lvalue, value}) = printf "%s = %s;" (show lvalue) (show value)
  show (ExprStmt {value, semicolon = True}) = printf "%s;" (show value)
  show (ExprStmt {value, semicolon = False}) = printf "%s" (show value)
  show (Return Nothing) = "return;"
  show (Return (Just value)) = printf "return %s;" (show value)
  show Break = "break;"
  show (Loop body) = printf "loop %s" (show body)
