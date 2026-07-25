{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module DW.Typechecker.Internal where

import DW.AST (SyntaxTree (..))
import DW.Common
import DW.Error (markSpan)
-- import DW.LexicalScopes hiding (lookupVariable)
-- import DW.LexicalScopes qualified as LexicalScopes
import DW.NameResolutionPass.Names
import DW.NamedAST (NST (..))
import DW.NamedAST qualified as N
import DW.TypedAST (TST (..), typeOf)
import DW.TypedAST qualified as T
import DW.Util

import Control.Monad (join)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Effectful.State.Static.Local (modifyM)

newtype Typechecker = Typechecker
  { variables :: HashMap VarName T.TypeExpr
  }

mkTypechecker :: Typechecker
mkTypechecker = Typechecker {variables = HashMap.empty}

bindVariable :: (State Typechecker :> es, Errors Err :> es) => VarName -> T.TypeExpr -> Eff es ()
bindVariable name ty = modifyM $ \c -> do
  variables <- HashMap.insert name ty <$> gets variables
  return $ c {variables}

lookupVariable :: (State Typechecker :> es) => VarName -> Eff es (Maybe T.TypeExpr)
lookupVariable name = HashMap.lookup name <$> gets variables

rewrap :: NST a -> TST a
rewrap (NST x span) = TST x span

convertValueTypeExpr :: N.ValueTypeExpr -> T.ValueTypeExpr
convertValueTypeExpr N.Any = T.Any
convertValueTypeExpr N.Void = T.Void
convertValueTypeExpr N.Bool = T.Bool
convertValueTypeExpr N.Int = T.Int
convertValueTypeExpr (N.Function params ret) = T.Function (map convertWrap params) (convertWrap ret)
  where
    convertWrap (NST typeExpr span) = TST (convertTypeExpr typeExpr) span

convertTypeExpr :: N.TypeExpr -> T.TypeExpr
convertTypeExpr (N.TypeExpr {reference, valueExpr}) = T.TypeExpr {reference, valueExpr = convertValueTypeExpr valueExpr}

convertOperator :: N.Operator -> T.Operator
convertOperator N.Or = T.Or
convertOperator N.And = T.And
convertOperator N.Equal = T.Equal
convertOperator N.NotEqual = T.NotEqual
convertOperator N.LessThan = T.LessThan
convertOperator N.LessThanOrEqual = T.LessThanOrEqual
convertOperator N.GreaterThan = T.GreaterThan
convertOperator N.GreaterThanOrEqual = T.GreaterThanOrEqual
convertOperator N.Plus = T.Plus
convertOperator N.Minus = T.Minus
convertOperator N.Multiply = T.Multiply
convertOperator N.Divide = T.Divide
convertOperator N.Not = T.Not
convertOperator N.Modulo = T.Modulo

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
  (HasCallStack, State Typechecker :> es, Errors Err :> es, Log :> es)
  => NST N.Body
  -> Eff es (TST T.Body)
typecheckBody (NST (N.Body stmts) span) = withRegion "Entering scope..." do
  tStmts <- forM stmts $ \stmt -> do
    tStmt <- typecheckStmt stmt
    -- A statement evaluates to the type of its final expression. If the final ExprStmt has a semicolon, then
    -- it's considered a statement, not an expression, and this "throws away" the value (so the type is void).
    case tStmt of
      (TST (T.ExprStmt {value, semicolon = False}) _) -> return (tStmt, typeOf (node value))
      _ -> return (tStmt, T.makeValueExpr T.Void)
  let (tStmts', retTypes) = unzip tStmts
      retType = safeLast retTypes `orElse` T.makeValueExpr T.Void

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
  (HasCallStack, State Typechecker :> es, Errors Err :> es, Log :> es)
  => NST N.Expr
  -> Eff es (TST T.Expr)
typecheckExpr (NST N.VoidLit span) = return $ TST T.VoidLit span
typecheckExpr (NST (N.BoolLit b) span) = return $ TST (T.BoolLit b) span
typecheckExpr (NST (N.IntLit n) span) = return $ TST (T.IntLit n) span
typecheckExpr (NST (N.Variable name) span) = do
  ty <- lookupVariable name <&> unwrapICE
  return $ TST (T.Variable ty name) span
typecheckExpr (NST (N.BinaryOperator op l r) span) = do
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
typecheckExpr (NST (N.UnaryOperator op value) span) = do
  tValue <- typecheckExpr value
  let op' = convertOperator op
      operandTy = typeOf (node tValue)
      outputTy = operationOutputs op'
  unless (operatorSupportsType op' operandTy) $ markSpan span (OperatorSupport (Text.show op) (Text.show operandTy))
  return $ TST (T.UnaryOperator outputTy op' tValue) span
typecheckExpr (NST (N.FunctionCall {function, arguments}) span) = do
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
typecheckExpr (NST (N.ExprBody body) span) = do
  tBody <- typecheckBody (NST body span)
  return $ TST (T.ExprBody (node tBody)) span
typecheckExpr (NST (N.IfThen condition body elseBody) span) = do
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
typecheckExpr (NST (N.Builtin name) span) = do
  let builtins = getBuiltins span
  ty <- lookup name builtins `orElseMarkSpan` (span, InvalidBuiltinName name, T.mkAny)
  return $ TST (T.Builtin ty name) span
typecheckExpr (NST (N.Lambda {params, returnType, body}) span) = do
  params' <- forM params $ \(ty, name) -> do
    let ty' = rewrap $ convertTypeExpr <$> ty
        name' = rewrap name
    bindVariable (node name) (node ty')
    return (ty', name')

  let returnType' = rewrap $ convertTypeExpr <$> returnType

  body' <- typecheckExpr body

  -- Make sure the lambda actually returns the annotated type
  when (node returnType' `doesNotUnify` typeOf (node body')) do
    markSpan (spanOf returnType') $ typeMismatch (typeOf (node body')) (node returnType')

  -- Calculate a type for the lambda
  let lambdaTy = T.makeValueExpr $ T.Function (map fst params') returnType'

  return $ TST (T.Lambda lambdaTy params' returnType' body') span

typeMismatch :: T.TypeExpr -> T.TypeExpr -> ErrorKind
typeMismatch expected got = TypeMismatch {expectedType = Text.show expected, gotType = Text.show got}

convertNST :: NST a -> TST a
convertNST (NST a span) = TST a span

typecheckLValue :: (State Typechecker :> es, Errors Err :> es) => NST N.LValue -> Eff es (TST T.LValue)
typecheckLValue (NST (N.LVariable name) span) = do
  ty <- lookupVariable name `orElseMarkSpanM` (span, UnboundVariable (getVarText name), T.mkAny)
  return $ TST (T.LVariable ty name) span

typecheckStmt ::
  (HasCallStack, State Typechecker :> es, Errors Err :> es, Log :> es)
  => NST N.Stmt
  -> Eff es (TST T.Stmt)
typecheckStmt (NST (N.Let {name, type_, value}) span) = do
  tValue <- typecheckExpr value

  -- If they provided a type annotation, then make sure it's correct
  maybeBoxedTValue <- forM type_ $ \type_ -> do
    let typeAnnotation = convertNST $ convertTypeExpr <$> type_
    let expectType = typeOf (node tValue)
    when (node typeAnnotation `doesNotUnify` expectType) do
      markSpan (spanOf value) (typeMismatch (node typeAnnotation) expectType)

    -- Also, if the type annotation is `any`, then we need to auto-box the value
    potentiallyBox (node typeAnnotation) `traverse` tValue

  let tValue' = maybeBoxedTValue `orElse` tValue

  -- If they provided a type annotation, then we take that as canonical.
  -- Otherwise, we use the inferred type.
  -- Generally they'll be the same, but subtyping introduces some subtlety here
  let boundTy = (convertNST <$> convertTypeExpr <$$> type_) `orElse` (typeOf <$> tValue')

  scribe $ format "Binding variable {} (type: {})" (Shown (node name), Shown type_)
  bindVariable (node name) (node boundTy)

  return $ TST (T.Let {name = convertNST name, type_ = boundTy, value = tValue'}) span
typecheckStmt (NST (N.Assign {lvalue, value}) span) = do
  tLValue <- typecheckLValue lvalue
  tValue <- typecheckExpr value
  let (T.LVariable tyL _) = node tLValue
      tyR = typeOf (node tValue)

  when (tyL `doesNotUnify` tyR) do
    markSpan (spanOf value) (typeMismatch tyL tyR)

  tValue' <- potentiallyBox tyL `traverse` tValue
  return $ TST (T.Assign tLValue tValue') span
typecheckStmt (NST (N.ExprStmt value semicolon) span) = do
  tValue <- typecheckExpr value
  return $ TST (T.ExprStmt tValue semicolon) span
typecheckStmt (NST (N.Return maybeValue) span) = do
  tMaybeValue <- traverse typecheckExpr maybeValue
  return $ TST (T.Return tMaybeValue) span
typecheckStmt (NST N.Break span) = return $ TST T.Break span
typecheckStmt (NST (N.Loop body) span) = do
  tBody <- typecheckBody body
  let (T.Body ty _) = node tBody
  when (ty `doesNotUnify` T.makeValueExpr T.Void) do
    markSpan (spanOf body) (typeMismatch (T.makeValueExpr T.Void) ty)
  return $ TST (T.Loop tBody) span

typecheckTopLevelStmtWithoutRecursing ::
  (HasCallStack, State Typechecker :> es, Errors Err :> es, Log :> es)
  => NST N.TopLevelStmt
  -> Eff es (TST VarName, TST T.TypeExpr)
typecheckTopLevelStmtWithoutRecursing (NST (N.TLet {name, ty, value}) _) = do
  valueTy <- case node value of
    N.VoidLit -> return T.mkVoid
    N.IntLit _ -> return T.mkInt
    N.BoolLit _ -> return T.mkBool
    N.Lambda {params, returnType} -> do
      let params' = convertNST . (convertTypeExpr <$>) . fst <$> params
      let returnType' = convertNST $ convertTypeExpr <$> returnType
      return $ T.TypeExpr {reference = False, valueExpr = T.Function params' returnType'}
    -- This should have been verified by the constexpr pass
    _ -> throwICE

  -- If they provided a type annotation, then make sure it's correct
  forM_ ty $ \type_ -> do
    let typeAnnotation = convertNST $ convertTypeExpr <$> type_
    when (node typeAnnotation `doesNotUnify` valueTy) do
      markSpan (spanOf value) (typeMismatch (node typeAnnotation) valueTy)

  -- We don't worry about auto-boxing because boxed values are forbidden at
  -- the top level (since they're dynamically allocated).
  -- This has already been verified by the ConstExprPass.

  -- If they provided a type annotation, then we take that as canonical.
  -- Otherwise, we use the inferred type.
  -- Generally they'll be the same, but subtyping introduces some subtlety here
  let boundTy = (convertNST <$> convertTypeExpr <$$> ty) `orElse` TST valueTy (spanOf value)

  scribe $ format "Binding top-level variable {} (type: {})" (Shown (node name), Shown boundTy)
  bindVariable (node name) (node boundTy)

  return (convertNST name, boundTy)

typecheckTopLevelStmtRecursively ::
  (HasCallStack, State Typechecker :> es, Errors Err :> es, Log :> es)
  => NST N.TopLevelStmt
  -> (TST VarName, TST T.TypeExpr)
  -> Eff es (TST T.TopLevelStmt)
typecheckTopLevelStmtRecursively (NST (N.TLet {value}) span) (name, ty) = do
  tValue <- typecheckExpr value
  return $ TST (T.TLet {name, ty, value = tValue}) span

typecheckTopLevel ::
  (HasCallStack, State Typechecker :> es, Errors Err :> es, Log :> es)
  => NST N.TopLevel
  -> Eff es (TST T.TopLevel)
typecheckTopLevel (NST (N.TopLevel stmts) span) = do
  -- Before recursing into the bodies of any functions, we first go through all the top-level
  -- bindings. This way the user can define mutually recursive functions rather than being restricted
  -- to top-to-bottom order.
  types <- forM stmts $ \stmt -> do
    typecheckTopLevelStmtWithoutRecursing stmt
  -- Then, we go through and actually evaluate the function bodies
  tStmts <- forM (stmts `zip` types) $ \(stmt, (name, boundTy)) -> do
    typecheckTopLevelStmtRecursively stmt (name, boundTy)
  return $ TST (T.TopLevel tStmts) span

runTypechecker ::
  (HasCallStack, Errors Err :> es, Log :> es)
  => NST N.TopLevel
  -> Eff es (TST T.TopLevel)
runTypechecker = evalState mkTypechecker . typecheckTopLevel
