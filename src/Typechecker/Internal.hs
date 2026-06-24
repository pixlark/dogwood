{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Typechecker.Internal where

import AST (AST (..), SyntaxTree (..))
import qualified AST as A
import Control.Applicative
import Control.Monad
import qualified Data.List
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful (Eff, runPureEff, (:>))
import Effectful.Error.Static
import Effectful.State.Static.Local
import Error
import Parser (parseStmt, runParse, runParseCallStack)
import TypedAST (TST (..), typeOf)
import qualified TypedAST as T
import Util

type LexicalScopes = NonEmpty [(Text, T.TypeExpr)]

lookupVariable' name (scope :| []) = lookup name scope
lookupVariable' name (scope :| rest) = (lookup name scope) <|> (lookupVariable' name (NE.fromList rest))

lookupVariable :: (State Typechecker :> es, Error Err :> es) => Span -> Text -> Eff es T.TypeExpr
{- HLINT ignore -}
lookupVariable span name = do
  scopes <- gets scopes
  case lookupVariable' name scopes of
    Just x -> return x
    Nothing -> throwSpan span (UnboundVariable name)

variableExists :: Text -> LexicalScopes -> Bool
variableExists name = isJust . lookupVariable' name

bindNewVariable :: Text -> T.TypeExpr -> LexicalScopes -> LexicalScopes
bindNewVariable name type_ (scope :| rest) =
  if isJust $ lookup name scope
    then
      let unboundScope = filter (\(n, _) -> n /= name) scope
       in ((name, type_) : unboundScope) :| rest
    else ((name, type_) : scope) :| rest

pushScope :: LexicalScopes -> LexicalScopes
pushScope = ([] <|)

popScope :: LexicalScopes -> Maybe LexicalScopes
popScope (_ :| []) = Nothing
popScope (_ :| rest) = Just $ NE.fromList rest

newtype Typechecker = Typechecker {scopes :: LexicalScopes}

makeTypechecker :: Typechecker
makeTypechecker = Typechecker {scopes = NE.fromList [[]]}

-- type Typechecker a = Except Err a
type TypecheckerE a = Eff '[State Typechecker, Error Err] a

convertValueTypeExpr :: A.ValueTypeExpr -> T.ValueTypeExpr
convertValueTypeExpr A.Void = T.Void
convertValueTypeExpr A.Bool = T.Bool
convertValueTypeExpr A.Int = T.Int
convertValueTypeExpr (A.NamespacedIdentifier parts) = T.NamespacedIdentifier parts
convertValueTypeExpr (A.Function params ret) = T.Function (map convertWrap params) (convertWrap ret)
  where
    convertWrap (AST typeExpr span) = TST (convertTypeExpr typeExpr) span

convertTypeExpr :: A.TypeExpr -> T.TypeExpr
convertTypeExpr (A.TypeExpr {reference, valueExpr}) = T.TypeExpr {reference, valueExpr = convertValueTypeExpr valueExpr}

convertOperator :: A.Operator -> T.Operator
convertOperator A.Or = T.Or
convertOperator A.And = T.And
convertOperator A.Equal = T.Equal
convertOperator A.NotEqual = T.NotEqual
convertOperator A.LessThan = T.LessThan
convertOperator A.LessThanOrEqual = T.LessThanOrEqual
convertOperator A.GreaterThan = T.GreaterThan
convertOperator A.GreaterThanOrEqual = T.GreaterThanOrEqual
convertOperator A.Plus = T.Plus
convertOperator A.Minus = T.Minus
convertOperator A.Multiply = T.Multiply
convertOperator A.Divide = T.Divide
convertOperator A.Not = T.Not

numericOperators :: [T.Operator]
numericOperators = [T.Plus, T.Minus, T.Multiply, T.Divide]

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

typecheckBody :: (State Typechecker :> es, Error Err :> es) => AST A.Body -> Eff es (TST T.Body)
typecheckBody (AST (A.Body stmts) span) = do
  tStmts <- forM stmts $ \stmt -> do
    tStmt <- typecheckStmt stmt
    -- A statement evaluates to the type of its final expression. If the final ExprStmt has a semicolon, then
    -- it's considered a statement, not an expression, and this "throws away" the value (so the type is void).
    case tStmt of
      (TST (T.ExprStmt {value, semicolon = False}) _) -> return $ (tStmt, typeOf (node value))
      _ -> return $ (tStmt, T.makeValueExpr T.Void)
  let (tStmts', retTypes) = unzip tStmts
      retType = safeLast retTypes `orElse` T.makeValueExpr T.Void
  return $ TST (T.Body retType tStmts') span

typecheckExpr :: (State Typechecker :> es, Error Err :> es) => AST A.Expr -> Eff es (TST T.Expr)
typecheckExpr (AST A.UndefinedLit span) = return $ TST T.UndefinedLit span
typecheckExpr (AST A.VoidLit span) = return $ TST T.VoidLit span
typecheckExpr (AST (A.BoolLit b) span) = return $ TST (T.BoolLit b) span
typecheckExpr (AST (A.IntLit n) span) = return $ TST (T.IntLit n) span
typecheckExpr (AST (A.Variable name) span) = do
  ty <- lookupVariable span name
  return $ TST (T.Variable ty name) span
typecheckExpr (AST (A.BinaryOperator op l r) span) = do
  tL <- typecheckExpr l
  tR <- typecheckExpr r
  let tyL = typeOf $ node tL
      tyR = typeOf $ node tR
  when (tyL `doesNotUnify` tyR) $ throwSpan span (typeMismatch tyL tyR)
  let op' = convertOperator op
      operandTy = typeOf (node tL)
      outputTy = operationOutputs op'
  when (not $ operatorSupportsType op' operandTy) $ throwSpan span (OperatorSupport (Text.show op) (Text.show operandTy))
  return $ TST (T.BinaryOperator outputTy op' tL tR) span
typecheckExpr (AST (A.UnaryOperator op value) span) = do
  tValue <- typecheckExpr value
  let op' = convertOperator op
      operandTy = typeOf (node tValue)
      outputTy = operationOutputs op'
  when (not $ operatorSupportsType op' operandTy) $ throwSpan span (OperatorSupport (Text.show op) (Text.show operandTy))
  return $ TST (T.UnaryOperator outputTy op' tValue) span
typecheckExpr (AST (A.FunctionCall {function, arguments}) span) = do
  tFunction <- typecheckExpr function
  let ty = typeOf (node tFunction)
  (params, ret) <- case ty of
    T.TypeExpr _ (T.Function params ret) -> return (params, ret)
    _ -> throwSpan span $ (CallingNonFunction (Text.show ty))
  when (length params /= length arguments) $ throwSpan span (WrongArgumentCount (length params) (length arguments))
  tArguments <- forM (arguments `zip` params) $ \(arg, param) -> do
    tArg <- typecheckExpr arg
    let tyArg = typeOf (node tArg)
    let tyParam = node param
    when (tyArg `doesNotUnify` tyParam) $ throwSpan (spanOf tArg) (typeMismatch tyParam tyArg)
    return tArg
  return $ TST (T.FunctionCall {type_ = node ret, function = tFunction, arguments = tArguments}) span
typecheckExpr (AST (A.ExprBody body) span) = do
  tBody <- typecheckBody (A.AST body span)
  return $ TST (T.ExprBody (node tBody)) span
typecheckExpr (AST (A.IfChain bodies elseBody) span) = do
  tBodies <- forM bodies $ \(condition, body) -> do
    tCondition <- typecheckExpr condition
    let tyCondition = typeOf (node tCondition)
    -- make sure all the conditions are actually boolean
    when (tyCondition `doesNotUnify` T.makeValueExpr T.Bool) $ throwSpan (spanOf tCondition) (typeMismatch (T.makeValueExpr T.Bool) tyCondition)
    tBody <- typecheckExpr body
    return (tCondition, tBody)
  tElseBody <- traverse typecheckExpr elseBody
  -- probably a less nasty way to do this, but I have a headache
  let tAllBodies = (snd <$> tBodies) `NE.appendList` ((Data.List.singleton <$> tElseBody) `orElse` [])
      -- if there's an else component to this if expression, then it's capable of evaluating to something other
      -- than void. if there's no else component, then all the bodies must evaluate to void (since the "default"
      -- else will evaluate to void).
      expectBodyType =
        if isJust tElseBody
          then typeOf $ node $ NE.head tAllBodies
          else T.makeValueExpr T.Void
  -- make sure all the bodies have the same type
  forM_ tAllBodies $ \body -> do
    let bodyType = typeOf $ node body
    when (bodyType `doesNotUnify` expectBodyType) $ throwSpan (spanOf body) (typeMismatch expectBodyType bodyType)
  return $ TST (T.IfChain expectBodyType tBodies tElseBody) span

typeMismatch :: T.TypeExpr -> T.TypeExpr -> ErrorKind
typeMismatch expected got = TypeMismatch {expectedType = Text.show expected, gotType = Text.show got}

throwSpan :: (Error Err :> es) => Span -> ErrorKind -> Eff es a
throwSpan span kind = throwError $ Err kind span

convertAST :: AST a -> TST a
convertAST (AST a span) = TST a span

typecheckLValue :: (State Typechecker :> es, Error Err :> es) => AST A.LValue -> Eff es (TST T.LValue)
typecheckLValue (A.AST (A.LVariable name) span) = do
  ty <- lookupVariable span name
  return $ TST (T.LVariable ty name) span

typecheckStmt :: (State Typechecker :> es, Error Err :> es) => AST A.Stmt -> Eff es (TST T.Stmt)
typecheckStmt (AST (A.Let {name, type_, value}) span) = do
  scopes <- gets scopes
  tValue <- typecheckExpr value
  let typeAnnotation = convertAST $ convertTypeExpr <$> type_
  let expectType = typeOf (node tValue)
  when (node typeAnnotation `doesNotUnify` expectType) $ throwSpan (spanOf value) (typeMismatch (node typeAnnotation) expectType)
  let scopes' = bindNewVariable (node name) (node typeAnnotation) scopes
  typechecker <- get
  put typechecker {scopes = scopes'}
  return $ TST (T.Let {name = convertAST name, type_ = typeAnnotation, value = tValue}) span
typecheckStmt (AST (A.Assign {lvalue, value}) span) = do
  tLValue <- typecheckLValue lvalue
  tValue <- typecheckExpr value
  let (T.LVariable tyL _) = (node tLValue)
      tyR = typeOf (node tValue)
  when (tyL `doesNotUnify` tyR) $ throwSpan (spanOf value) (typeMismatch tyL tyR)
  return $ TST (T.Assign tLValue tValue) span
typecheckStmt (AST (A.ExprStmt value semicolon) span) = do
  tValue <- typecheckExpr value
  return $ TST (T.ExprStmt tValue semicolon) span
typecheckStmt (AST (A.Return maybeValue) span) = do
  tMaybeValue <- traverse typecheckExpr maybeValue
  return $ TST (T.Return tMaybeValue) span
typecheckStmt (AST A.Break span) = return $ TST T.Break span
typecheckStmt (AST (A.Loop body) span) = do
  tBody <- typecheckBody body
  let (T.Body ty _) = node tBody
  when (ty `doesNotUnify` T.makeValueExpr T.Void) $ throwSpan (spanOf body) (typeMismatch (T.makeValueExpr T.Void) ty)
  return $ TST (T.Loop tBody) span

runTypecheck :: Text -> Result (TST T.Stmt)
runTypecheck source = case result of
  Left e -> Left e
  Right a -> runPureEff $ runErrorNoCallStack $ evalState makeTypechecker $ typecheckStmt a
  where
    result = runParse source parseStmt

runTypecheckCallStack :: Text -> Either (CallStack, Err) (TST T.Stmt)
runTypecheckCallStack source = case result of
  Left e -> Left e
  Right a -> runPureEff $ runError $ evalState makeTypechecker $ typecheckStmt a
  where
    result = runParseCallStack source parseStmt
