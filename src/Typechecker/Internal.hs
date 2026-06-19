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
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
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

newtype Context = Context {scopes :: LexicalScopes}

type Typechecker a = Except Err a

convertValueTypeExpr :: A.ValueTypeExpr -> T.ValueTypeExpr
convertValueTypeExpr A.Void = T.Void
convertValueTypeExpr A.Bool = T.Bool
convertValueTypeExpr A.Int = T.Int
convertValueTypeExpr (A.NamespacedIdentifier parts) = T.NamespacedIdentifier parts

convertTypeExpr :: A.TypeExpr -> T.TypeExpr
convertTypeExpr (A.TypeExpr {reference, valueExpr}) = T.TypeExpr {reference, valueExpr = convertValueTypeExpr valueExpr}

typecheckExpr :: Context -> AST A.Expr -> Typechecker (TST T.Expr)
typecheckExpr context (AST A.VoidLit span) = return $ TST T.VoidLit span
typecheckExpr _ _ = undefined

typeMismatch :: T.TypeExpr -> T.TypeExpr -> ErrorKind
typeMismatch expected got = TypeMismatch {expected = Text.show expected, got = Text.show got}

throwSpan :: Span -> ErrorKind -> Typechecker a
throwSpan span kind = throwE $ Err kind span

convertAST :: AST a -> TST a
convertAST (AST a span) = TST a span

typecheckStmt :: Context -> AST A.Stmt -> Typechecker (TST T.Stmt, Context)
typecheckStmt (context@Context {scopes}) (AST (A.Let {name, type_, value}) span) = do
  tValue <- typecheckExpr context value
  let typeAnnotation = convertAST $ convertTypeExpr <$> type_
  let expectType = typeOf (node tValue)
  when (node typeAnnotation /= expectType) $ throwSpan (spanOf value) (typeMismatch (node typeAnnotation) expectType)
  let scopes' = bindNewVariable (node name) (node typeAnnotation) scopes
  return $ (TST (T.Let {name = convertAST name, type_ = typeAnnotation, value = tValue}) span, Context {scopes = scopes'})
typecheckStmt _ _ = undefined
