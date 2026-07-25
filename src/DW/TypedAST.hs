module DW.TypedAST where

import DW.AST (SyntaxTree (..))
import DW.Common hiding (Writer, execWriter, tell)
import DW.NameResolutionPass.Names

import Control.Monad.Trans.Writer (execWriter, tell)
import Data.List (intersperse)
import Data.Text qualified as T

data TST a = TST a Span

instance (Eq a) => Eq (TST a) where
  (TST a _) == (TST b _) = a == b

instance Functor TST where
  fmap f (TST x span) = TST (f x) span

instance Foldable TST where
  foldMap f (TST x _) = f x

instance Traversable TST where
  traverse :: (Applicative f) => (a -> f b) -> TST a -> f (TST b)
  traverse f (TST x span) = (\x -> TST x span) <$> f x

instance SyntaxTree TST where
  node (TST x _) = x
  spanOf (TST _ s) = s

data ValueTypeExpr = Any | Void | Bool | Int | Function [TST TypeExpr] (TST TypeExpr)
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
  | Variable TypeExpr VarName
  | BinaryOperator TypeExpr Operator (TST Expr) (TST Expr)
  | UnaryOperator TypeExpr Operator (TST Expr)
  | FunctionCall {type_ :: TypeExpr, function :: TST Expr, arguments :: [TST Expr]}
  | ExprBody Body
  | IfThen TypeExpr (TST Expr) (TST Expr) (TST Expr)
  | Builtin TypeExpr Text
  | Boxed TypeExpr Expr
  | Lambda {lambdaTy :: TypeExpr, params :: [(TST TypeExpr, TST VarName)], returnType :: TST TypeExpr, body :: TST Expr}
  | NewOperator {newTy :: TST TypeExpr, arguments :: [TST Expr]}
  deriving (Eq)

makeValueExpr :: ValueTypeExpr -> TypeExpr
makeValueExpr valueExpr = TypeExpr {reference = False, valueExpr}

makeReferenceExpr :: ValueTypeExpr -> TypeExpr
makeReferenceExpr valueExpr = TypeExpr {reference = True, valueExpr}

mkAny = makeValueExpr Any

mkVoid = makeValueExpr Void

mkBool = makeValueExpr Bool

mkInt = makeValueExpr Int

typeOf :: Expr -> TypeExpr
typeOf VoidLit = makeValueExpr Void
typeOf (BoolLit _) = makeValueExpr Bool
typeOf (IntLit _) = makeValueExpr Int
typeOf (Variable t _) = t
typeOf (BinaryOperator t _ _ _) = t
typeOf (UnaryOperator t _ _) = t
typeOf (FunctionCall {type_}) = type_
typeOf (ExprBody (Body t _)) = t
typeOf (IfThen t _ _ _) = t
typeOf (Builtin t _) = t
-- the type annotation on Boxed isn't the type of the expression (a boxed expression is always of
-- the Any type), but rather the type of the interior, boxed expression
typeOf (Boxed _ _) = mkAny
typeOf (Lambda t _ _ _) = t
-- The new operator is annotated with the _value_ type that it allocates, but the expression
-- actually _evaluates_ to a reference type. This should be verified by the typechecking phase.
typeOf (NewOperator (TST (TypeExpr {reference = False, valueExpr}) _) _) = TypeExpr {reference = True, valueExpr}
typeOf (NewOperator _ _) = throwICE

data LValue = LVariable TypeExpr VarName
  deriving (Eq)

data Body = Body TypeExpr [TST Stmt]
  deriving (Eq)

data Stmt
  = Let {name :: TST VarName, type_ :: TST TypeExpr, value :: TST Expr}
  | Assign {lvalue :: TST LValue, value :: TST Expr}
  | ExprStmt {value :: TST Expr, semicolon :: Bool}
  | Return (Maybe (TST Expr))
  | Break
  | Loop (TST Body)
  deriving (Eq)

data TopLevelStmt = TLet {name :: TST VarName, ty :: TST TypeExpr, value :: TST Expr}

newtype TopLevel = TopLevel [TST TopLevelStmt]

data AnyAST = AnyTopLevel TopLevel | AnyTopLevelStmt TopLevelStmt | AnyStmt Stmt | AnyExpr Expr | AnyLValue LValue | AnyBody Body | AnyTypeExpr TypeExpr | AnyText T.Text
  deriving (Show)

instance (Show a) => Show (TST a) where
  show (TST value _) = show value

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
  show (BinaryOperator t op a b) = printf "(%s %s %s : %s)" (show a) (show op) (show b) (show t)
  show (UnaryOperator t op e) = printf "(%s%s : %s)" (show op) (show e) (show t)
  show (FunctionCall {type_, function, arguments}) = execWriter $ do
    tell "("
    case function of
      (TST (Variable _ name) _) -> tell $ T.unpack $ getVarText name
      _ -> tell $ printf "(%s)" (show function)
    tell "("
    forM_ (take (length arguments - 1) arguments) $ \arg -> do
      tell $ show arg
      tell ", "
    tell $ show $ last arguments
    tell $ printf ") : %s)" (show type_)
  show (Variable t sym) = printf "(%s : %s)" (show sym) (show t)
  show (ExprBody body) = show body
  show (IfThen ty condition body elseBody) = execWriter $ do
    tell "(if "
    tell $ show condition
    tell " "
    tell $ show body
    tell " else "
    tell $ show elseBody
    tell ") : "
    tell $ show ty
  show (Builtin ty name) = printf "builtin %s : %s" name (show ty)
  show (Boxed _ e) = show e
  show (Lambda {params, returnType, body}) = execWriter $ do
    tell "fn("
    forM_ (intersperse Nothing $ Just <$> params) $ \m -> do
      case m of
        Nothing -> tell ", "
        Just (ty, name) -> do
          tell $ T.unpack $ getVarText $ node name
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

instance Show LValue where
  show (LVariable _ name) = T.unpack $ getVarText name

instance Show Body where
  show (Body t stmts) = execWriter $ do
    tell "{\n"
    forM_ stmts $ \stmt -> do
      tell (show stmt)
      tell "\n"
    tell $ printf "} : %s" (show t)

instance Show Stmt where
  show (Let {name = (TST name _), type_, value}) = printf "let %s: %s = %s;" (T.unpack $ getVarText name) (show type_) (show value)
  show (Assign {lvalue, value}) = printf "%s = %s;" (show lvalue) (show value)
  show (ExprStmt {value, semicolon = True}) = printf "%s;" (show value)
  show (ExprStmt {value, semicolon = False}) = printf "%s" (show value)
  show (Return Nothing) = "return;"
  show (Return (Just value)) = printf "return %s;" (show value)
  show Break = "break;"
  show (Loop body) = printf "loop %s" (show body)

instance Show TopLevelStmt where
  show (TLet {name = (TST name _), ty, value}) = printf "let %s: %s = %s;" (T.unpack $ getVarText name) (show ty) (show value)

instance Show TopLevel where
  show (TopLevel stmts) = execWriter $ do
    forM_ stmts $ \stmt -> do
      tell $ show stmt
      tell "\n"
