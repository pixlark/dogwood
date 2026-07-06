{-# LANGUAGE RankNTypes #-}

module DW.TypedAST.Visit
  ( Visitor (..),
    defaultVisitor,
    runStmtVisitor,
    runExprVisitor,
    runLValueVisitor,
    runTypeExprVisitor,
    runValueTypeExprVisitor,
    runBodyVisitor,
    runTopLevelStmtVisitor,
    runTopLevelVisitor,
  )
where

import DW.Common
import DW.TypedAST

data Visitor m = Visitor
  { onValueTypeExpr :: TST ValueTypeExpr -> m () -> m (),
    onTypeExpr :: TST TypeExpr -> m () -> m (),
    onExpr :: TST Expr -> m () -> m (),
    onStmt :: TST Stmt -> m () -> m (),
    onLValue :: TST LValue -> m () -> m (),
    onBody :: TST Body -> m () -> m (),
    onTopLevelStmt :: TST TopLevelStmt -> m () -> m (),
    onTopLevel :: TST TopLevel -> m () -> m ()
  }

defaultVisitor :: (Monad m) => Visitor m
defaultVisitor =
  Visitor
    { onValueTypeExpr = \_ children -> children,
      onTypeExpr = \_ children -> children,
      onExpr = \_ children -> children,
      onStmt = \_ children -> children,
      onLValue = \_ children -> children,
      onBody = \_ children -> children,
      onTopLevelStmt = \_ children -> children,
      onTopLevel = \_ children -> children
    }

runValueTypeExprVisitor :: (Monad m) => Visitor m -> TST ValueTypeExpr -> m ()
runValueTypeExprVisitor v vte@(TST (Function params ret) _) =
  onValueTypeExpr v vte $ do
    forM_ params (runTypeExprVisitor v)
    runTypeExprVisitor v ret
runValueTypeExprVisitor v vte = onValueTypeExpr v vte (return ())

runTypeExprVisitor :: (Monad m) => Visitor m -> TST TypeExpr -> m ()
runTypeExprVisitor v te@(TST (TypeExpr _ valueExpr) span_) =
  onTypeExpr v te $
    runValueTypeExprVisitor v (TST valueExpr span_)

runLValueVisitor :: (Monad m) => Visitor m -> TST LValue -> m ()
runLValueVisitor v lv@(TST (LVariable typeExpr _) span_) =
  onLValue v lv $
    runTypeExprVisitor v (TST typeExpr span_)

runBodyVisitor :: (Monad m) => Visitor m -> TST Body -> m ()
runBodyVisitor v body@(TST (Body typeExpr stmts) span_) =
  onBody v body $ do
    runTypeExprVisitor v (TST typeExpr span_)
    forM_ stmts (runStmtVisitor v)

runExprVisitor :: (Monad m) => Visitor m -> TST Expr -> m ()
runExprVisitor v expr@(TST UndefinedLit _) = onExpr v expr (return ())
runExprVisitor v expr@(TST VoidLit _) = onExpr v expr (return ())
runExprVisitor v expr@(TST (BoolLit _) _) = onExpr v expr (return ())
runExprVisitor v expr@(TST (IntLit _) _) = onExpr v expr (return ())
runExprVisitor v expr@(TST (Variable typeExpr _) span_) =
  onExpr v expr $
    runTypeExprVisitor v (TST typeExpr span_)
runExprVisitor v expr@(TST (BinaryOperator typeExpr _ left right) span_) =
  onExpr v expr $ do
    runTypeExprVisitor v (TST typeExpr span_)
    runExprVisitor v left
    runExprVisitor v right
runExprVisitor v expr@(TST (UnaryOperator typeExpr _ operand) span_) =
  onExpr v expr $ do
    runTypeExprVisitor v (TST typeExpr span_)
    runExprVisitor v operand
runExprVisitor v expr@(TST (FunctionCall type_ function arguments) span_) =
  onExpr v expr $ do
    runTypeExprVisitor v (TST type_ span_)
    runExprVisitor v function
    forM_ arguments (runExprVisitor v)
runExprVisitor v expr@(TST (ExprBody body) span_) =
  onExpr v expr $
    runBodyVisitor v (TST body span_)
runExprVisitor v expr@(TST (IfThen typeExpr condition body elseBody) span_) =
  onExpr v expr $ do
    runTypeExprVisitor v (TST typeExpr span_)
    runExprVisitor v condition
    runExprVisitor v body
    runExprVisitor v elseBody
runExprVisitor v expr@(TST (Builtin typeExpr _) span) =
  onExpr v expr do
    runTypeExprVisitor v (TST typeExpr span)
runExprVisitor v expr@(TST (Boxed typeExpr innerExpr) span) =
  onExpr v expr do
    runTypeExprVisitor v (TST typeExpr span)
    runExprVisitor v (TST innerExpr span)
runExprVisitor v expr@(TST (Lambda {ty, params, returnType, body}) span) =
  onExpr v expr do
    runTypeExprVisitor v (TST ty span)
    forM_ params $ \(ty, _) -> do
      runTypeExprVisitor v ty
    runTypeExprVisitor v returnType
    runExprVisitor v body

runStmtVisitor :: (Monad m) => Visitor m -> TST Stmt -> m ()
runStmtVisitor v stmt@(TST (Let _ type_ value) _) =
  onStmt v stmt $ do
    runTypeExprVisitor v type_
    runExprVisitor v value
runStmtVisitor v stmt@(TST (Assign lvalue value) _) =
  onStmt v stmt $ do
    runLValueVisitor v lvalue
    runExprVisitor v value
runStmtVisitor v stmt@(TST (ExprStmt value _) _) =
  onStmt v stmt $
    runExprVisitor v value
runStmtVisitor v stmt@(TST (Return maybeValue) _) =
  onStmt v stmt $
    forM_ maybeValue (runExprVisitor v)
runStmtVisitor v stmt@(TST Break _) =
  onStmt v stmt (return ())
runStmtVisitor v stmt@(TST (Loop body) _) =
  onStmt v stmt $
    runBodyVisitor v body

runTopLevelStmtVisitor :: (Monad m) => Visitor m -> TST TopLevelStmt -> m ()
runTopLevelStmtVisitor v tls@(TST (TLet {ty, value}) _) = do
  onTopLevelStmt v tls do
    runTypeExprVisitor v ty
    runExprVisitor v value

runTopLevelVisitor :: (Monad m) => Visitor m -> TST TopLevel -> m ()
runTopLevelVisitor v tl@(TST (TopLevel stmts) _) = do
  onTopLevel v tl do
    forM_ stmts $ \stmt -> do
      runTopLevelStmtVisitor v stmt
