-- {-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DW.LexicalScopes (LexicalScopes, HasLexicalScopes (..), lookupVariable, lookupByValue, variableExists, bindNewVariable, pushScope, popScope, mkScopes) where

import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import DW.Common
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE

type LexicalScopes a = NonEmpty [(Text, a)]

class HasLexicalScopes a s | s -> a where
  getScopes :: s -> LexicalScopes a
  setScopes :: LexicalScopes a -> s -> s

lookupVariablePure :: Text -> LexicalScopes a -> Maybe a
lookupVariablePure name (scope :| []) = lookup name scope
lookupVariablePure name (scope :| rest) = lookup name scope <|> lookupVariablePure name (NE.fromList rest)

lookupVariable :: (State s :> es, HasLexicalScopes a s) => Text -> Eff es (Maybe a)
lookupVariable name = do
  scopes <- gets getScopes
  return $ lookupVariablePure name scopes

lookupByValueInScope :: (a -> Bool) -> [(Text, a)] -> Eff es (Maybe a)
lookupByValueInScope predicate scope = case filter (predicate . snd) scope of
  [] -> return Nothing
  [(_, x)] -> return (Just x)
  _ -> throwICE

lookupByValue :: (a -> Bool) -> LexicalScopes a -> Eff es (Maybe a)
lookupByValue predicate (scope :| []) = lookupByValueInScope predicate scope
lookupByValue predicate (scope :| rest) =
  runMaybeT $
    MaybeT (lookupByValueInScope predicate scope)
      <|> MaybeT (lookupByValue predicate (NE.fromList rest))

variableExists :: (State s :> es, HasLexicalScopes a s) => Text -> Eff es Bool
variableExists name = isJust <$> lookupVariable name

bindNewVariable :: (State s :> es, HasLexicalScopes a s) => Text -> a -> Eff es ()
bindNewVariable name val = do
  scopes <- gets getScopes
  let scopes' = bindNewVariable' name val scopes
  modify (setScopes scopes')
  where
    bindNewVariable' name val (scope :| rest) =
      if isJust $ lookup name scope
        then
          let unboundScope = filter (\(n, _) -> n /= name) scope
           in ((name, val) : unboundScope) :| rest
        else ((name, val) : scope) :| rest

pushScope :: (State s :> es, HasLexicalScopes a s) => Eff es ()
pushScope = do
  scopes <- gets getScopes
  let scopes' = [] <| scopes
  modify (setScopes scopes')

popScope :: (State s :> es, HasLexicalScopes a s) => Eff es (Maybe ())
popScope = do
  scopes <- gets getScopes
  let scopes' = popScope' scopes
  case scopes' of
    Just scopes' -> do modify (setScopes scopes'); return (Just ())
    Nothing -> return Nothing
  where
    popScope' (_ :| []) = Nothing
    popScope' (_ :| rest) = Just $ NE.fromList rest

mkScopes :: LexicalScopes a
mkScopes = NE.fromList [[]]
