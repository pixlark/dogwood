module LexicalScopes where

import Common
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import TypedAST

type LexicalScopes a = NonEmpty [(Text, a)]

lookupVariable' :: Text -> LexicalScopes a -> Maybe a
lookupVariable' name (scope :| []) = lookup name scope
lookupVariable' name (scope :| rest) = lookup name scope <|> lookupVariable' name (NE.fromList rest)

lookupVariable :: (State (LexicalScopes a) :> es, Error Err :> es) => Span -> Text -> Eff es a
lookupVariable span name = do
  scopes <- get
  case lookupVariable' name scopes of
    Just x -> return x
    Nothing -> throwSpan span (UnboundVariable name)

variableExists :: Text -> LexicalScopes a -> Bool
variableExists name = isJust . lookupVariable' name

bindNewVariable :: Text -> a -> LexicalScopes a -> LexicalScopes a
bindNewVariable name val (scope :| rest) =
  if isJust $ lookup name scope
    then
      let unboundScope = filter (\(n, _) -> n /= name) scope
       in ((name, val) : unboundScope) :| rest
    else ((name, val) : scope) :| rest

pushScope :: LexicalScopes a -> LexicalScopes a
pushScope = ([] <|)

popScope :: LexicalScopes a -> Maybe (LexicalScopes a)
popScope (_ :| []) = Nothing
popScope (_ :| rest) = Just $ NE.fromList rest

mkScopes :: LexicalScopes a
mkScopes = NE.fromList [[]]
