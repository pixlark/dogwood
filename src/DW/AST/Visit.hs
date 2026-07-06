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
runTypeExprVisitor v te@(AST (TypeExpr _ valueExpr) span) =
  onTypeExpr v te $
    runValueTypeExprVisitor v (AST valueExpr span)

runLValueVisitor :: (Monad m) => Visitor m -> AST LValue -> m ()
runLValueVisitor v lv@(AST (LVariable _) _) =
  onLValue v lv $
    return ()

runBodyVisitor :: (Monad m) => Visitor m -> AST Body -> m ()
runBodyVisitor v body@(AST (Body stmts) _) =
  onBody v body $ do
    forM_ stmts (runStmtVisitor v)

runExprVisitor :: (Monad m) => Visitor m -> AST Expr -> m ()
runExprVisitor v expr@(AST UndefinedLit _) = onExpr v expr (return ())
runExprVisitor v expr@(AST VoidLit _) = onExpr v expr (return ())
runExprVisitor v expr@(AST (BoolLit _) _) = onExpr v expr (return ())
runExprVisitor v expr@(AST (IntLit _) _) = onExpr v expr (return ())
runExprVisitor v expr@(AST (Variable _) _) =
  onExpr v expr $
    return ()
runExprVisitor v expr@(AST (BinaryOperator _ left right) _) =
  onExpr v expr $ do
    runExprVisitor v left
    runExprVisitor v right
runExprVisitor v expr@(AST (UnaryOperator _ operand) _) =
  onExpr v expr $ do
    runExprVisitor v operand
runExprVisitor v expr@(AST (FunctionCall function arguments) _) =
  onExpr v expr $ do
    runExprVisitor v function
    forM_ arguments (runExprVisitor v)
runExprVisitor v expr@(AST (ExprBody body) span) =
  onExpr v expr $
    runBodyVisitor v (AST body span)
runExprVisitor v expr@(AST (IfChain bodies elseBody) _) =
  onExpr v expr $ do
    forM_ bodies $ \(condition, body) -> do
      runExprVisitor v condition
      runExprVisitor v body
    forM_ elseBody $ runExprVisitor v
runExprVisitor v expr@(AST (Builtin _) _) =
  onExpr v expr do
    return ()
runExprVisitor v expr@(AST (Lambda {params, returnType, body}) _) = do
  onExpr v expr do
    forM_ params $ \(ty, _) -> do
      runTypeExprVisitor v ty
    forM_ returnType $ \returnType -> do
      runTypeExprVisitor v returnType
    runExprVisitor v body

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
