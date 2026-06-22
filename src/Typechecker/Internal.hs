{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Typechecker.Internal where

import AST (AST (..), SyntaxTree (..))
import qualified AST as A
import Control.Applicative
import Control.Monad
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful (Eff, (:>))
import Effectful.Error.Static
import Effectful.State.Static.Local
import Error
import TypedAST (TST (..), typeOf)
import qualified TypedAST as T
import Util

type LexicalScopes = NonEmpty [(Text, T.TypeExpr)]

lookupVariable :: Text -> LexicalScopes -> Maybe T.TypeExpr
{- HLINT ignore -}
lookupVariable name (scope :| []) = lookup name scope
lookupVariable name (scope :| rest) = (lookup name scope) <|> (lookupVariable name (NE.fromList rest))

variableExists :: Text -> LexicalScopes -> Bool
variableExists name = isJust . lookupVariable name

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

-- type Typechecker a = Except Err a
type TypecheckerE a = Eff '[State Typechecker, Error Err] a

convertValueTypeExpr :: A.ValueTypeExpr -> T.ValueTypeExpr
convertValueTypeExpr A.Void = T.Void
convertValueTypeExpr A.Bool = T.Bool
convertValueTypeExpr A.Int = T.Int
convertValueTypeExpr (A.NamespacedIdentifier parts) = T.NamespacedIdentifier parts

convertTypeExpr :: A.TypeExpr -> T.TypeExpr
convertTypeExpr (A.TypeExpr {reference, valueExpr}) = T.TypeExpr {reference, valueExpr = convertValueTypeExpr valueExpr}

typecheckExpr :: AST A.Expr -> TypecheckerE (TST T.Expr)
typecheckExpr (AST A.VoidLit span) = return $ TST T.VoidLit span
typecheckExpr _ = undefined

typeMismatch :: T.TypeExpr -> T.TypeExpr -> ErrorKind
typeMismatch expected got = TypeMismatch {expected = Text.show expected, got = Text.show got}

throwSpan :: Span -> ErrorKind -> TypecheckerE a
throwSpan span kind = throwError $ Err kind span

convertAST :: AST a -> TST a
convertAST (AST a span) = TST a span

typecheckStmt :: AST A.Stmt -> TypecheckerE (TST T.Stmt)
typecheckStmt (AST (A.Let {name, type_, value}) span) = do
  scopes <- gets scopes
  tValue <- typecheckExpr value
  let typeAnnotation = convertAST $ convertTypeExpr <$> type_
  let expectType = typeOf (node tValue)
  when (node typeAnnotation /= expectType) $ throwSpan (spanOf value) (typeMismatch (node typeAnnotation) expectType)
  let scopes' = bindNewVariable (node name) (node typeAnnotation) scopes
  typechecker <- get
  put typechecker {scopes = scopes'}
  return $ TST (T.Let {name = convertAST name, type_ = typeAnnotation, value = tValue}) span
typecheckStmt _ = undefined
