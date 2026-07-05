module DW.LowerPass (runLowerPass) where

import DW.AST qualified as AST
import DW.LoweredAST qualified as L
import DW.Util
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE

lowerAST :: AST.AST a -> (a -> b) -> L.LST b
lowerAST (AST.AST x span) f = L.LST (f x) span

lowerValueTypeExpr :: AST.ValueTypeExpr -> L.ValueTypeExpr
lowerValueTypeExpr AST.Any = L.Any
lowerValueTypeExpr AST.Void = L.Void
lowerValueTypeExpr AST.Bool = L.Bool
lowerValueTypeExpr AST.Int = L.Int
lowerValueTypeExpr (AST.NamespacedIdentifier parts) = L.NamespacedIdentifier parts
lowerValueTypeExpr (AST.Function params ret) =
  L.Function (map lowerTypeExpr params) (lowerTypeExpr ret)

lowerTypeExpr :: AST.AST AST.TypeExpr -> L.LST L.TypeExpr
lowerTypeExpr ast = lowerAST ast $ \(AST.TypeExpr ref val) ->
  L.TypeExpr ref (lowerValueTypeExpr val)

lowerOperator :: AST.Operator -> L.Operator
lowerOperator AST.Or = L.Or
lowerOperator AST.And = L.And
lowerOperator AST.Equal = L.Equal
lowerOperator AST.NotEqual = L.NotEqual
lowerOperator AST.LessThan = L.LessThan
lowerOperator AST.LessThanOrEqual = L.LessThanOrEqual
lowerOperator AST.GreaterThan = L.GreaterThan
lowerOperator AST.GreaterThanOrEqual = L.GreaterThanOrEqual
lowerOperator AST.Plus = L.Plus
lowerOperator AST.Minus = L.Minus
lowerOperator AST.Multiply = L.Multiply
lowerOperator AST.Divide = L.Divide
lowerOperator AST.Not = L.Not
lowerOperator AST.Modulo = L.Modulo

lowerExpr :: AST.AST AST.Expr -> L.LST L.Expr
lowerExpr (AST.AST (AST.IfChain ((condition, body) :| []) elseBody) span) =
  L.LST (L.IfThen condition' body' elseBody') span
  where
    condition' = lowerExpr condition
    body' = lowerExpr body
    elseBody' = (lowerExpr <$> elseBody) `orElse` L.LST L.VoidLit span
lowerExpr (AST.AST (AST.IfChain ((condition, body) :| rest) elseBody) span) =
  L.LST (L.IfThen condition' body' elseBody') span
  where
    condition' = lowerExpr condition
    body' = lowerExpr body
    elseIfChain' = AST.AST (AST.IfChain (NE.fromList rest) elseBody) span
    elseBody' = lowerExpr elseIfChain'
lowerExpr ast = lowerAST ast $ \case
  AST.UndefinedLit -> L.UndefinedLit
  AST.VoidLit -> L.VoidLit
  AST.BoolLit b -> L.BoolLit b
  AST.IntLit n -> L.IntLit n
  AST.Variable name -> L.Variable name
  AST.BinaryOperator op left right ->
    L.BinaryOperator (lowerOperator op) (lowerExpr left) (lowerExpr right)
  AST.UnaryOperator op operand ->
    L.UnaryOperator (lowerOperator op) (lowerExpr operand)
  AST.FunctionCall fn args ->
    L.FunctionCall (lowerExpr fn) (map lowerExpr args)
  AST.ExprBody body -> L.ExprBody (lowerBody body)
  AST.IfChain _ _ -> error "unreachable"
  AST.Builtin name -> L.Builtin name

lowerLValue :: AST.AST AST.LValue -> L.LST L.LValue
lowerLValue ast = lowerAST ast $ \(AST.LVariable name) -> L.LVariable name

lowerBody :: AST.Body -> L.Body
lowerBody (AST.Body stmts) = L.Body (map lowerStmt stmts)

lowerStmt :: AST.AST AST.Stmt -> L.LST L.Stmt
lowerStmt ast = lowerAST ast $ \case
  AST.Let name ty val ->
    L.Let (lowerAST name id) (lowerTypeExpr <$> ty) (lowerExpr val)
  AST.Assign lval val ->
    L.Assign (lowerLValue lval) (lowerExpr val)
  AST.ExprStmt val semi ->
    L.ExprStmt (lowerExpr val) semi
  AST.Return maybeVal ->
    L.Return (fmap lowerExpr maybeVal)
  AST.Break -> L.Break
  AST.Loop body ->
    L.Loop (lowerAST body lowerBody)

runLowerPass :: AST.AST AST.Stmt -> L.LST L.Stmt
runLowerPass = lowerStmt
