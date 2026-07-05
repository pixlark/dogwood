{-# LANGUAGE RankNTypes #-}

module DW.AST.Visit
  ( Visitor (..),
    defaultVisitor,
    runStmtVisitor,
    runExprVisitor,
    runLValueVisitor,
    runTypeExprVisitor,
    runValueTypeExprVisitor,
    runBodyVisitor,
  )
where

import DW.AST
import DW.Common

data Visitor m = Visitor
  { onValueTypeExpr :: AST ValueTypeExpr -> m () -> m (),
    onTypeExpr :: AST TypeExpr -> m () -> m (),
    onExpr :: AST Expr -> m () -> m (),
    onStmt :: AST Stmt -> m () -> m (),
    onLValue :: AST LValue -> m () -> m (),
    onBody :: AST Body -> m () -> m ()
  }

defaultVisitor :: (Monad m) => Visitor m
defaultVisitor =
  Visitor
    { onValueTypeExpr = \_ children -> children,
      onTypeExpr = \_ children -> children,
      onExpr = \_ children -> children,
      onStmt = \_ children -> children,
      onLValue = \_ children -> children,
      onBody = \_ children -> children
    }

runValueTypeExprVisitor :: (Monad m) => Visitor m -> AST ValueTypeExpr -> m ()
runValueTypeExprVisitor v vte@(AST (Function params ret) _) =
  onValueTypeExpr v vte $ do
    forM_ params (runTypeExprVisitor v)
    runTypeExprVisitor v ret
runValueTypeExprVisitor v vte = onValueTypeExpr v vte (return ())

runTypeExprVisitor :: (Monad m) => Visitor m -> AST TypeExpr -> m ()
runTypeExprVisitor v te@(AST (TypeExpr _ valueExpr) span_) =
  onTypeExpr v te $
    runValueTypeExprVisitor v (AST valueExpr span_)

runLValueVisitor :: (Monad m) => Visitor m -> AST LValue -> m ()
runLValueVisitor v lv@(AST (LVariable _) span_) =
  onLValue v lv $
    return ()

runBodyVisitor :: (Monad m) => Visitor m -> AST Body -> m ()
runBodyVisitor v body@(AST (Body stmts) span_) =
  onBody v body $ do
    forM_ stmts (runStmtVisitor v)

runExprVisitor :: (Monad m) => Visitor m -> AST Expr -> m ()
runExprVisitor v expr@(AST UndefinedLit _) = onExpr v expr (return ())
runExprVisitor v expr@(AST VoidLit _) = onExpr v expr (return ())
runExprVisitor v expr@(AST (BoolLit _) _) = onExpr v expr (return ())
runExprVisitor v expr@(AST (IntLit _) _) = onExpr v expr (return ())
runExprVisitor v expr@(AST (Variable _) span_) =
  onExpr v expr $
    return ()
runExprVisitor v expr@(AST (BinaryOperator _ left right) span_) =
  onExpr v expr $ do
    runExprVisitor v left
    runExprVisitor v right
runExprVisitor v expr@(AST (UnaryOperator _ operand) span_) =
  onExpr v expr $ do
    runExprVisitor v operand
runExprVisitor v expr@(AST (FunctionCall function arguments) span_) =
  onExpr v expr $ do
    runExprVisitor v function
    forM_ arguments (runExprVisitor v)
runExprVisitor v expr@(AST (ExprBody body) span_) =
  onExpr v expr $
    runBodyVisitor v (AST body span_)
runExprVisitor v expr@(AST (IfChain bodies elseBody) span_) =
  onExpr v expr $ do
    forM_ bodies $ \(condition, body) -> do
      runExprVisitor v condition
      runExprVisitor v body
    forM_ elseBody $ runExprVisitor v
runExprVisitor v expr@(AST (Builtin _) span) =
  onExpr v expr do
    return ()

runStmtVisitor :: (Monad m) => Visitor m -> AST Stmt -> m ()
runStmtVisitor v stmt@(AST (Let _ type_ value) _) =
  onStmt v stmt $ do
    forM_ type_ $ runTypeExprVisitor v
    runExprVisitor v value
runStmtVisitor v stmt@(AST (Assign lvalue value) _) =
  onStmt v stmt $ do
    runLValueVisitor v lvalue
    runExprVisitor v value
runStmtVisitor v stmt@(AST (ExprStmt value _) _) =
  onStmt v stmt $
    runExprVisitor v value
runStmtVisitor v stmt@(AST (Return maybeValue) _) =
  onStmt v stmt $
    forM_ maybeValue (runExprVisitor v)
runStmtVisitor v stmt@(AST Break _) =
  onStmt v stmt (return ())
runStmtVisitor v stmt@(AST (Loop body) _) =
  onStmt v stmt $
    runBodyVisitor v body
