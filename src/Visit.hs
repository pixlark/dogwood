{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Visit
  ( Visitor (..),
    runStmtVisitor,
    runExprVisitor,
    runLValueVisitor,
    runTypeExprVisitor,
    runValueTypeExprVisitor,
    runBodyVisitor,
  )
where

import Control.Monad (forM_)
import qualified Data.List.NonEmpty as NE
import Effectful
import Effectful.State.Static.Local (State)
import TypedAST

class Visitor v where
  visitValueTypeExpr :: (State v :> es) => TST ValueTypeExpr -> Eff es ()
  visitValueTypeExpr _ = return ()
  visitTypeExpr :: (State v :> es) => TST TypeExpr -> Eff es ()
  visitTypeExpr _ = return ()
  visitExpr :: (State v :> es) => TST Expr -> Eff es ()
  visitExpr _ = return ()
  visitStmt :: (State v :> es) => TST Stmt -> Eff es ()
  visitStmt _ = return ()
  visitLValue :: (State v :> es) => TST LValue -> Eff es ()
  visitLValue _ = return ()
  visitBody :: (State v :> es) => TST Body -> Eff es ()
  visitBody _ = return ()

runValueTypeExprVisitor :: (Visitor v, State v :> es) => TST ValueTypeExpr -> Eff es ()
runValueTypeExprVisitor vte@(TST (Function params ret) _) = do
  visitValueTypeExpr vte
  forM_ params runTypeExprVisitor
  runTypeExprVisitor ret
runValueTypeExprVisitor vte = visitValueTypeExpr vte

runTypeExprVisitor :: (Visitor v, State v :> es) => TST TypeExpr -> Eff es ()
runTypeExprVisitor te@(TST (TypeExpr _ valueExpr) span_) = do
  visitTypeExpr te
  runValueTypeExprVisitor (TST valueExpr span_)

runLValueVisitor :: (Visitor v, State v :> es) => TST LValue -> Eff es ()
runLValueVisitor lv@(TST (LVariable typeExpr _) span_) = do
  visitLValue lv
  runTypeExprVisitor (TST typeExpr span_)

runBodyVisitor :: (Visitor v, State v :> es) => TST Body -> Eff es ()
runBodyVisitor body@(TST (Body typeExpr stmts) span_) = do
  visitBody body
  runTypeExprVisitor (TST typeExpr span_)
  forM_ stmts runStmtVisitor

runExprVisitor :: (Visitor v, State v :> es) => TST Expr -> Eff es ()
runExprVisitor expr@(TST UndefinedLit _) = visitExpr expr
runExprVisitor expr@(TST VoidLit _) = visitExpr expr
runExprVisitor expr@(TST (BoolLit _) _) = visitExpr expr
runExprVisitor expr@(TST (IntLit _) _) = visitExpr expr
runExprVisitor expr@(TST (Variable typeExpr _) span_) = do
  visitExpr expr
  runTypeExprVisitor (TST typeExpr span_)
runExprVisitor expr@(TST (BinaryOperator typeExpr _ left right) span_) = do
  visitExpr expr
  runTypeExprVisitor (TST typeExpr span_)
  runExprVisitor left
  runExprVisitor right
runExprVisitor expr@(TST (UnaryOperator typeExpr _ operand) span_) = do
  visitExpr expr
  runTypeExprVisitor (TST typeExpr span_)
  runExprVisitor operand
runExprVisitor expr@(TST (FunctionCall type_ function arguments) span_) = do
  visitExpr expr
  runTypeExprVisitor (TST type_ span_)
  runExprVisitor function
  forM_ arguments runExprVisitor
runExprVisitor expr@(TST (ExprBody body) span_) = do
  visitExpr expr
  runBodyVisitor (TST body span_)
runExprVisitor expr@(TST (IfChain typeExpr branches elseBody) span_) = do
  visitExpr expr
  runTypeExprVisitor (TST typeExpr span_)
  forM_ (NE.toList branches) $ \(condition, body) -> do
    runExprVisitor condition
    runExprVisitor body
  forM_ elseBody runExprVisitor

runStmtVisitor :: (Visitor v, State v :> es) => TST Stmt -> Eff es ()
runStmtVisitor stmt@(TST (Let _ type_ value) _) = do
  visitStmt stmt
  runTypeExprVisitor type_
  runExprVisitor value
runStmtVisitor stmt@(TST (Assign lvalue value) _) = do
  visitStmt stmt
  runLValueVisitor lvalue
  runExprVisitor value
runStmtVisitor stmt@(TST (ExprStmt value _) _) = do
  visitStmt stmt
  runExprVisitor value
runStmtVisitor stmt@(TST (Return maybeValue) _) = do
  visitStmt stmt
  forM_ maybeValue runExprVisitor
runStmtVisitor stmt@(TST Break _) = do
  visitStmt stmt
runStmtVisitor stmt@(TST (Loop body) _) = do
  visitStmt stmt
  runBodyVisitor body
