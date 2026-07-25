{-# LANGUAGE MultiParamTypeClasses #-}

module DW.NameResolutionPass where

import DW.AST (SyntaxTree (node))
import DW.Common
import DW.Error (markSpan)
import DW.LexicalScopes
import DW.LoweredAST (LST (..))
import DW.LoweredAST qualified as L
import DW.NameResolutionPass.Names
import DW.NamedAST (NST (..))
import DW.NamedAST qualified as N
import DW.Util (ifM_, whenM)

import Data.List (nub)

data NameResolver = NameResolver
  { varCounter :: Int,
    variables :: LexicalScopes VarName
  }

instance HasLexicalScopes VarName NameResolver where
  getScopes = variables
  setScopes v n = n {variables = v}

mkNameResolver :: NameResolver
mkNameResolver = NameResolver {varCounter = 0, variables = mkScopes}

mkVar :: (State NameResolver :> es) => Text -> Eff es VarName
mkVar text = do
  id <- gets varCounter
  modify (\n -> n {varCounter = id + 1})
  return VarName {id, text}

resolveOperator :: L.Operator -> N.Operator
resolveOperator L.Or = N.Or
resolveOperator L.And = N.And
resolveOperator L.Equal = N.Equal
resolveOperator L.NotEqual = N.NotEqual
resolveOperator L.LessThan = N.LessThan
resolveOperator L.LessThanOrEqual = N.LessThanOrEqual
resolveOperator L.GreaterThan = N.GreaterThan
resolveOperator L.GreaterThanOrEqual = N.GreaterThanOrEqual
resolveOperator L.Plus = N.Plus
resolveOperator L.Minus = N.Minus
resolveOperator L.Multiply = N.Multiply
resolveOperator L.Divide = N.Divide
resolveOperator L.Not = N.Not
resolveOperator L.Modulo = N.Modulo

resolveValueTypeExpr :: (State NameResolver :> es, Errors Err :> es) => L.ValueTypeExpr -> Eff es N.ValueTypeExpr
resolveValueTypeExpr L.Any = return N.Any
resolveValueTypeExpr L.Void = return N.Void
resolveValueTypeExpr L.Bool = return N.Bool
resolveValueTypeExpr L.Int = return N.Int
resolveValueTypeExpr (L.Function params ret) = do
  nParams <- mapM resolveType params
  nRet <- resolveType ret
  return $ N.Function nParams nRet

resolveType :: (State NameResolver :> es, Errors Err :> es) => LST L.TypeExpr -> Eff es (NST N.TypeExpr)
resolveType (LST (L.TypeExpr {reference, valueExpr}) span) = do
  nValueExpr <- resolveValueTypeExpr valueExpr
  return $ NST (N.TypeExpr {reference, valueExpr = nValueExpr}) span

resolveBody :: (State NameResolver :> es, Errors Err :> es) => LST L.Body -> Eff es (NST N.Body)
resolveBody (LST (L.Body stmts) span) = do
  pushScope
  nStmts <- mapM resolveStmt stmts
  popScope <&> unwrapICE
  return $ NST (N.Body nStmts) span

resolveStmt :: (State NameResolver :> es, Errors Err :> es) => LST L.Stmt -> Eff es (NST N.Stmt)
resolveStmt (LST (L.Let {name = (LST text textSpan), type_, value}) span) = do
  nTy <- resolveType `traverse` type_
  nValue <- resolveExpr value
  -- We bind the name _after_ evaluating the expression so that shadowing works
  -- correctly. Basically, we want the following to work:
  -- let x = 5;
  -- let x = x + 1;
  -- and if we bind the name beforehand, the latter `let` becomes recursive.
  name <- mkVar text
  bindNewVariable text name
  return $ NST (N.Let {name = NST name textSpan, type_ = nTy, value = nValue}) span
resolveStmt (LST (L.Assign (LST (L.LVariable text) lValueSpan) value) span) = do
  nValue <- resolveExpr value
  name <- lookupVariable text
  name <- case name of
    Nothing -> do
      markSpan lValueSpan (UnboundVariable text)
      -- Mint a fresh name just so we have something to continue with, in case
      -- there are other errors the user might want to know about
      name <- mkVar text
      return name
    Just name -> return name
  let nLValue = NST (N.LVariable name) lValueSpan
  return $ NST (N.Assign {lvalue = nLValue, value = nValue}) span
resolveStmt (LST (L.ExprStmt {value, semicolon}) span) = do
  nValue <- resolveExpr value
  return $ NST (N.ExprStmt {value = nValue, semicolon}) span
resolveStmt (LST (L.Return maybeValue) span) = do
  nMaybeValue <- resolveExpr `traverse` maybeValue
  return $ NST (N.Return nMaybeValue) span
resolveStmt (LST L.Break span) = return $ NST N.Break span
resolveStmt (LST (L.Loop body) span) = do
  nBody <- resolveBody body
  return $ NST (N.Loop nBody) span

resolveExpr :: (State NameResolver :> es, Errors Err :> es) => LST L.Expr -> Eff es (NST N.Expr)
resolveExpr (LST L.VoidLit span) = return $ NST N.VoidLit span
resolveExpr (LST (L.BoolLit b) span) = return $ NST (N.BoolLit b) span
resolveExpr (LST (L.IntLit n) span) = return $ NST (N.IntLit n) span
resolveExpr (LST (L.Variable text) span) = do
  name <- lookupVariable text
  name <- case name of
    Nothing -> do
      markSpan span (UnboundVariable text)
      -- Mint a fresh name just so we have something to continue with, in case
      -- there are other errors the user might want to know about
      name <- mkVar text
      return name
    Just name -> return name
  return $ NST (N.Variable name) span
resolveExpr (LST (L.BinaryOperator op a b) span) = do
  nA <- resolveExpr a
  nB <- resolveExpr b
  return $ NST (N.BinaryOperator (resolveOperator op) nA nB) span
resolveExpr (LST (L.UnaryOperator op e) span) = do
  nE <- resolveExpr e
  return $ NST (N.UnaryOperator (resolveOperator op) nE) span
resolveExpr (LST (L.FunctionCall {function, arguments}) span) = do
  nFunction <- resolveExpr function
  nArguments <- mapM resolveExpr arguments
  return $ NST (N.FunctionCall {function = nFunction, arguments = nArguments}) span
resolveExpr (LST (L.ExprBody body) span) = do
  nBody <- resolveBody (LST body span)
  return $ NST (N.ExprBody (node nBody)) span
resolveExpr (LST (L.IfThen cond then_ else_) span) = do
  nCond <- resolveExpr cond
  nThen <- resolveExpr then_
  nElse <- resolveExpr else_
  return $ NST (N.IfThen nCond nThen nElse) span
resolveExpr (LST (L.Builtin name) span) = return $ NST (N.Builtin name) span
resolveExpr (LST (L.Lambda {params, returnType, body}) span) = do
  savedVars <- gets variables
  let newVars = rootScope savedVars
  modify (\n -> n {variables = newVars})

  nParams <- forM params $ \(ty, LST text textSpan) -> do
    nTy <- resolveType ty
    whenM (isBoundInThisScope text) do
      markSpan textSpan (DuplicateParameterNames text)
    name <- mkVar text
    bindNewVariable text name
    return (nTy, NST name textSpan)
  nReturnType <- resolveType returnType
  nBody <- resolveExpr body

  modify (\n -> n {variables = savedVars})

  return $ NST (N.Lambda nParams nReturnType nBody) span

resolveTopLevel ::
  (State NameResolver :> es, Errors Err :> es)
  => LST L.TopLevel -> Eff es (NST N.TopLevel)
resolveTopLevel (LST (L.TopLevel stmts) span) = do
  -- Go through all the top-level statements without recursing _first_.
  -- Otherwise, we couldn't define top-level recursive functions
  forM_ stmts $ \(LST (L.TLet (L.LST text textSpan) _ _) _) -> do
    name <- mkVar text
    ifM_
      (variableExists text)
      (markSpan textSpan (MultipleDefinitionsOfTLVariable text))
      do bindNewVariable text name
  -- Then actually go through their definitions.
  nStmts <- forM stmts $ \(LST (L.TLet (L.LST text textSpan) ty value) span) -> do
    pushScope
    nValue <- resolveExpr value
    popScope <&> unwrapICE
    name <- lookupVariable text <&> unwrapICE
    nTy <- resolveType `traverse` ty
    return $ NST (N.TLet (N.NST name textSpan) nTy nValue) span
  return $ NST (N.TopLevel nStmts) span

runNameResolution :: LST L.TopLevel -> Result (NST N.TopLevel)
runNameResolution = runPureEff . runErrorsNoCallStack . evalState mkNameResolver . resolveTopLevel
