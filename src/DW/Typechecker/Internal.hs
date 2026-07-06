{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module DW.Typechecker.Internal where

import DW.AST (SyntaxTree (..))
import DW.Common
import DW.Error (markSpan)
import DW.LexicalScopes
import DW.LoweredAST (LST (..))
import DW.LoweredAST qualified as L
import DW.TypedAST (TST (..), typeOf)
import DW.TypedAST qualified as T
import DW.Util
import Data.Text qualified as Text

newtype Typechecker = Typechecker {scopes :: LexicalScopes T.TypeExpr}

instance HasLexicalScopes T.TypeExpr Typechecker where
  getScopes = scopes
  setScopes scopes t = t {scopes}

mkTypechecker :: Typechecker
mkTypechecker = Typechecker {scopes = mkScopes}

rewrap :: LST a -> TST a
rewrap (LST x span) = TST x span

convertValueTypeExpr :: L.ValueTypeExpr -> T.ValueTypeExpr
convertValueTypeExpr L.Any = T.Any
convertValueTypeExpr L.Void = T.Void
convertValueTypeExpr L.Bool = T.Bool
convertValueTypeExpr L.Int = T.Int
convertValueTypeExpr (L.NamespacedIdentifier parts) = T.NamespacedIdentifier parts
convertValueTypeExpr (L.Function params ret) = T.Function (map convertWrap params) (convertWrap ret)
  where
    convertWrap (LST typeExpr span) = TST (convertTypeExpr typeExpr) span

convertTypeExpr :: L.TypeExpr -> T.TypeExpr
convertTypeExpr (L.TypeExpr {reference, valueExpr}) = T.TypeExpr {reference, valueExpr = convertValueTypeExpr valueExpr}

convertOperator :: L.Operator -> T.Operator
convertOperator L.Or = T.Or
convertOperator L.And = T.And
convertOperator L.Equal = T.Equal
convertOperator L.NotEqual = T.NotEqual
convertOperator L.LessThan = T.LessThan
convertOperator L.LessThanOrEqual = T.LessThanOrEqual
convertOperator L.GreaterThan = T.GreaterThan
convertOperator L.GreaterThanOrEqual = T.GreaterThanOrEqual
convertOperator L.Plus = T.Plus
convertOperator L.Minus = T.Minus
convertOperator L.Multiply = T.Multiply
convertOperator L.Divide = T.Divide
convertOperator L.Not = T.Not
convertOperator L.Modulo = T.Modulo

numericOperators :: [T.Operator]
numericOperators = [T.Plus, T.Minus, T.Multiply, T.Divide, T.Modulo]

booleanOperators :: [T.Operator]
booleanOperators = [T.Or, T.And, T.Not]

universalOperators :: [T.Operator]
universalOperators = [T.Equal, T.NotEqual, T.LessThan, T.LessThanOrEqual, T.GreaterThan, T.GreaterThanOrEqual]

operationOutputs :: T.Operator -> T.TypeExpr
operationOutputs T.Or = T.makeValueExpr T.Bool
operationOutputs T.And = T.makeValueExpr T.Bool
operationOutputs T.Equal = T.makeValueExpr T.Bool
operationOutputs T.NotEqual = T.makeValueExpr T.Bool
operationOutputs T.LessThan = T.makeValueExpr T.Bool
operationOutputs T.LessThanOrEqual = T.makeValueExpr T.Bool
operationOutputs T.GreaterThan = T.makeValueExpr T.Bool
operationOutputs T.GreaterThanOrEqual = T.makeValueExpr T.Bool
operationOutputs T.Plus = T.makeValueExpr T.Int
operationOutputs T.Minus = T.makeValueExpr T.Int
operationOutputs T.Multiply = T.makeValueExpr T.Int
operationOutputs T.Divide = T.makeValueExpr T.Int
operationOutputs T.Not = T.makeValueExpr T.Bool
operationOutputs T.Modulo = T.makeValueExpr T.Int

isPrimitive :: T.TypeExpr -> Bool
isPrimitive T.TypeExpr {reference = True} = False
isPrimitive T.TypeExpr {valueExpr = T.Void} = True
isPrimitive T.TypeExpr {valueExpr = T.Bool} = True
isPrimitive T.TypeExpr {valueExpr = T.Int} = True
isPrimitive _ = False

operatorSupportsType :: T.Operator -> T.TypeExpr -> Bool
operatorSupportsType _ (T.TypeExpr {valueExpr = T.Any}) = True
operatorSupportsType op ty =
  (op `elem` universalOperators && isPrimitive ty)
    || (op `elem` numericOperators && ty == T.makeValueExpr T.Int)
    || (op `elem` booleanOperators && ty == T.makeValueExpr T.Bool)

unifies :: T.TypeExpr -> T.TypeExpr -> Bool
-- the `any` type unifies with everything
unifies (T.TypeExpr {valueExpr = T.Any}) _ = True
unifies _ (T.TypeExpr {valueExpr = T.Any}) = True
-- otherwise, types only unify with themselves (no subtyping yet, besides `any`)
unifies t1 t2 = t1 == t2

doesNotUnify :: T.TypeExpr -> T.TypeExpr -> Bool
doesNotUnify t1 t2 = not (t1 `unifies` t2)

typecheckBody ::
  (HasCallStack, State Typechecker :> es, Errors Err :> es, Log :> es) =>
  LST L.Body ->
  Eff es (TST T.Body)
typecheckBody (LST (L.Body stmts) span) = withRegion "Entering scope..." do
  -- each body opens a new lexical scope
  pushScope

  tStmts <- forM stmts $ \stmt -> do
    tStmt <- typecheckStmt stmt
    -- A statement evaluates to the type of its final expression. If the final ExprStmt has a semicolon, then
    -- it's considered a statement, not an expression, and this "throws away" the value (so the type is void).
    case tStmt of
      (TST (T.ExprStmt {value, semicolon = False}) _) -> return (tStmt, typeOf (node value))
      _ -> return (tStmt, T.makeValueExpr T.Void)
  let (tStmts', retTypes) = unzip tStmts
      retType = safeLast retTypes `orElse` T.makeValueExpr T.Void

  -- close the lexical scope
  popScope `orICEM` span
  return $ TST (T.Body retType tStmts') span

getBuiltins :: Span -> [(Text, T.TypeExpr)]
getBuiltins span =
  [ ("print", T.makeValueExpr $ T.Function [TST T.mkAny span] (TST T.mkVoid span))
  ]

potentiallyBox :: (HasCallStack, Log :> es) => T.TypeExpr -> T.Expr -> Eff es T.Expr
potentiallyBox (T.TypeExpr {valueExpr = T.Any}) e = do
  scribe $ format "{}" (Only (Shown e))
  if typeOf e == T.mkAny
    then return e -- already boxed
    else do
      scribe $ format "Boxing expression {} to coerce to any (original type: {})" (Shown e, Shown (typeOf e))
      return $ T.Boxed (typeOf e) e
potentiallyBox _ e = return e

typecheckExpr ::
  (HasCallStack, State Typechecker :> es, Errors Err :> es, Log :> es) =>
  LST L.Expr ->
  Eff es (TST T.Expr)
typecheckExpr (LST L.UndefinedLit span) = do
  scribe "Encountered undefined value - boxing it"
  return $ TST (T.Boxed T.mkUndefined T.UndefinedLit) span
typecheckExpr (LST L.VoidLit span) = return $ TST T.VoidLit span
typecheckExpr (LST (L.BoolLit b) span) = return $ TST (T.BoolLit b) span
typecheckExpr (LST (L.IntLit n) span) = return $ TST (T.IntLit n) span
typecheckExpr (LST (L.Variable name) span) = do
  ty <- lookupVariable name `orElseMarkSpanM` (span, UnboundVariable name, T.mkAny)
  return $ TST (T.Variable ty name) span
typecheckExpr (LST (L.BinaryOperator op l r) span) = do
  tL <- typecheckExpr l
  tR <- typecheckExpr r
  let tyL = typeOf $ node tL
      tyR = typeOf $ node tR
  when (tyL `doesNotUnify` tyR) $ markSpan span (typeMismatch tyL tyR)
  let op' = convertOperator op
      operandTy = typeOf (node tL)
      outputTy = operationOutputs op'
  unless (operatorSupportsType op' operandTy) $ markSpan span (OperatorSupport (Text.show op) (Text.show operandTy))
  return $ TST (T.BinaryOperator outputTy op' tL tR) span
typecheckExpr (LST (L.UnaryOperator op value) span) = do
  tValue <- typecheckExpr value
  let op' = convertOperator op
      operandTy = typeOf (node tValue)
      outputTy = operationOutputs op'
  unless (operatorSupportsType op' operandTy) $ markSpan span (OperatorSupport (Text.show op) (Text.show operandTy))
  return $ TST (T.UnaryOperator outputTy op' tValue) span
typecheckExpr (LST (L.FunctionCall {function, arguments}) span) = do
  -- Make sure that the thing we're calling is actually a function
  tFunction <- typecheckExpr function
  let ty = typeOf (node tFunction)
  (params, ret) <- case ty of
    T.TypeExpr _ (T.Function params ret) -> return (params, ret)
    _ -> do
      markSpan span $ CallingNonFunction (Text.show ty)
      -- We pretend like the function exists with the right number of arguments,
      -- where every type is `any`, so that we can continue trying to typecheck
      -- despite there being an error
      return (replicate (length arguments) (TST T.mkAny span), TST T.mkAny span)
  -- Make sure we've passed the right number of arguments
  when (length params /= length arguments) do
    -- If we didn't, we'll just typecheck the first `min (length params) (length arguments)`
    -- arguments, since that's all we can really do.
    markSpan span (WrongArgumentCount (length params) (length arguments))
  -- Also check the type of each argument
  tArguments <- forM (arguments `zip` params) $ \(arg, param) -> do
    tArg <- typecheckExpr arg
    let tyArg = typeOf (node tArg)
    let tyParam = node param
    when (tyArg `doesNotUnify` tyParam) $ markSpan (spanOf tArg) (typeMismatch tyParam tyArg)
    -- If a function's parameter has the type "any", then the value passed through that argument
    -- will get coerced to "any", meaning it has to be boxed up.
    tArg' <- potentiallyBox tyParam `traverse` tArg
    return tArg'
  return $ TST (T.FunctionCall {type_ = node ret, function = tFunction, arguments = tArguments}) span
typecheckExpr (LST (L.ExprBody body) span) = do
  tBody <- typecheckBody (LST body span)
  return $ TST (T.ExprBody (node tBody)) span
typecheckExpr (LST (L.IfThen condition body elseBody) span) = do
  tCondition <- typecheckExpr condition

  let tyCondition = typeOf (node tCondition)
  when (tyCondition `doesNotUnify` T.makeValueExpr T.Bool) do
    markSpan (spanOf tCondition) (typeMismatch (T.makeValueExpr T.Bool) tyCondition)

  tBody <- typecheckExpr body
  tElseBody <- typecheckExpr elseBody

  let tyBody = typeOf (node tBody)
      tyElseBody = typeOf (node tElseBody)
  when (tyBody `doesNotUnify` tyElseBody) do
    markSpan (spanOf tElseBody) (typeMismatch tyBody tyElseBody)

  return $ TST (T.IfThen tyBody tCondition tBody tElseBody) span
typecheckExpr (LST (L.Builtin name) span) = do
  let builtins = getBuiltins span
  ty <- lookup name builtins `orElseMarkSpan` (span, InvalidBuiltinName name, T.mkAny)
  return $ TST (T.Builtin ty name) span
typecheckExpr (LST (L.Lambda {params, returnType, body}) span) = do
  -- We don't have closures yet, so while we're typechecking the body, we need to
  -- have an empty typing environment
  savedScopes <- gets scopes
  modify (\t -> t {scopes = mkScopes})

  params' <- forM params $ \(ty, name) -> do
    let ty' = rewrap $ convertTypeExpr <$> ty
        name' = rewrap name
    bindNewVariable (node name) (node ty')
    return (ty', name')

  let returnType' = rewrap $ convertTypeExpr <$> returnType

  body' <- typecheckExpr body

  -- Make sure the lambda actually returns the annotated type
  when (node returnType' `doesNotUnify` typeOf (node body')) do
    markSpan (spanOf returnType') $ typeMismatch (typeOf (node body')) (node returnType')

  -- Restore the saved scopes
  modify (\t -> t {scopes = savedScopes})

  -- Calculate a type for the lambda
  let lambdaTy = T.makeValueExpr $ T.Function (map fst params') returnType'

  return $ TST (T.Lambda lambdaTy params' returnType' body') span

typeMismatch :: T.TypeExpr -> T.TypeExpr -> ErrorKind
typeMismatch expected got = TypeMismatch {expectedType = Text.show expected, gotType = Text.show got}

convertLST :: LST a -> TST a
convertLST (LST a span) = TST a span

typecheckLValue :: (State Typechecker :> es, Errors Err :> es) => LST L.LValue -> Eff es (TST T.LValue)
typecheckLValue (LST (L.LVariable name) span) = do
  ty <- lookupVariable name `orElseMarkSpanM` (span, UnboundVariable name, T.mkAny)
  return $ TST (T.LVariable ty name) span

-- typecheckLet :: (State Typechecker :> es, Errors Err :> es)
--   => (LST Text, Maybe (LST L.TypeExpr), LST L.Expr)

typecheckStmt ::
  (HasCallStack, State Typechecker :> es, Errors Err :> es, Log :> es) =>
  LST L.Stmt ->
  Eff es (TST T.Stmt)
typecheckStmt (LST (L.Let {name, type_, value}) span) = do
  tValue <- typecheckExpr value

  scribe $ format "{} : {}" (Shown tValue, Shown (typeOf $ node tValue))

  -- If they provided a type annotation, then make sure it's correct
  maybeBoxedTValue <- forM type_ $ \type_ -> do
    let typeAnnotation = convertLST $ convertTypeExpr <$> type_
    let expectType = typeOf (node tValue)
    when (node typeAnnotation `doesNotUnify` expectType) do
      markSpan (spanOf value) (typeMismatch (node typeAnnotation) expectType)

    -- Also, if the type annotation is `any`, then we need to auto-box the value
    potentiallyBox (node typeAnnotation) `traverse` tValue

  let tValue' = maybeBoxedTValue `orElse` tValue

  -- If they provided a type annotation, then we take that as canonical.
  -- Otherwise, we use the inferred type.
  -- Generally they'll be the same, but subtyping introduces some subtlety here
  let boundTy = (convertLST <$> convertTypeExpr <$$> type_) `orElse` (typeOf <$> tValue')

  scribe $ format "Binding variable {} (type: {})" (node name, Shown type_)
  bindNewVariable (node name) (node boundTy)

  return $ TST (T.Let {name = convertLST name, type_ = boundTy, value = tValue'}) span
typecheckStmt (LST (L.Assign {lvalue, value}) span) = do
  tLValue <- typecheckLValue lvalue
  tValue <- typecheckExpr value
  let (T.LVariable tyL _) = node tLValue
      tyR = typeOf (node tValue)

  when (tyL `doesNotUnify` tyR) do
    markSpan (spanOf value) (typeMismatch tyL tyR)

  tValue' <- potentiallyBox tyL `traverse` tValue
  return $ TST (T.Assign tLValue tValue') span
typecheckStmt (LST (L.ExprStmt value semicolon) span) = do
  tValue <- typecheckExpr value
  return $ TST (T.ExprStmt tValue semicolon) span
typecheckStmt (LST (L.Return maybeValue) span) = do
  tMaybeValue <- traverse typecheckExpr maybeValue
  return $ TST (T.Return tMaybeValue) span
typecheckStmt (LST L.Break span) = return $ TST T.Break span
typecheckStmt (LST (L.Loop body) span) = do
  tBody <- typecheckBody body
  let (T.Body ty _) = node tBody
  when (ty `doesNotUnify` T.makeValueExpr T.Void) do
    markSpan (spanOf body) (typeMismatch (T.makeValueExpr T.Void) ty)
  return $ TST (T.Loop tBody) span

typecheckTopLevelStmt ::
  (HasCallStack, State Typechecker :> es, Errors Err :> es, Log :> es) =>
  LST L.TopLevelStmt ->
  Eff es (TST T.TopLevelStmt)
typecheckTopLevelStmt (LST (L.TLet {name, ty, value}) span) = undefined

typecheckTopLevel ::
  (HasCallStack, State Typechecker :> es, Errors Err :> es, Log :> es) =>
  LST L.TopLevel ->
  Eff es (TST T.TopLevel)
typecheckTopLevel = undefined

runTypechecker ::
  (HasCallStack, Errors Err :> es, Log :> es) =>
  LST L.TopLevel ->
  Eff es (TST T.TopLevel)
runTypechecker = evalState mkTypechecker . typecheckTopLevel
