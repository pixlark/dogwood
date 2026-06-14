{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Internal where

import AST (NamespacedIdentifier)
import qualified AST as A
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Lexer
import Text.Printf

data Parser = Parser {current :: Token, lexer :: Lexer}
  deriving (Show)

type ParserM a = ExceptT String (State Parser) a

type Result a = Either String a

advance :: ParserM ()
advance = ExceptT $ state $ \parser -> case runState nextToken parser.lexer of
  (Right tok, lexer') -> (Right (), parser {current = tok, lexer = lexer'})
  (Left e, lexer') -> (Left e, parser {lexer = lexer'})

makeParser :: Lexer -> Result Parser
makeParser lexer = parser' <$ result
  where
    parser = Parser {current = Eof, lexer}
    -- "prime the pump"
    (result, parser') = runState (runExceptT advance) parser

expectKeyword :: Text -> ParserM ()
expectKeyword keyword = do
  current <- gets current
  if current == Keyword keyword
    then advance
    else throwE $ printf "Expected keyword %s" keyword

expectGlyph :: Text -> ParserM ()
expectGlyph glyph = do
  current <- gets current
  if current == Glyph glyph
    then advance
    else throwE $ printf "Expected glyph %s" glyph

readSymbol :: ParserM Text
readSymbol = do
  current <- gets current
  case current of
    Symbol sym -> do advance; return sym
    _ -> throwE "Expected symbol"

matchKeyword :: Text -> ParserM Bool
matchKeyword keyword = do
  current <- gets current
  if current == Keyword keyword
    then do advance; return True
    else return False

matchGlyph :: Text -> ParserM Bool
matchGlyph glyph = do
  current <- gets current
  if current == Glyph glyph
    then do advance; return True
    else return False

parseBuiltinType :: ParserM A.BuiltinType
parseBuiltinType = do
  current <- gets current
  case current of
    Keyword "void" -> return A.Void
    Keyword "bool" -> return A.Bool
    Keyword "int" -> return A.Int
    _ -> throwE "Expected builtin type"

data SeparatorConfig a = SeparatorConfig
  { trailing :: Bool,
    separator :: Token,
    consume :: ParserM (Maybe a)
  }

parseSeparatedSequence :: forall a. SeparatorConfig a -> ParserM [a]
parseSeparatedSequence SeparatorConfig {trailing, separator, consume} = parseSeparatedSequence' [] False
  where
    parseSeparatedSequence' :: [a] -> Bool -> ParserM [a]
    parseSeparatedSequence' sequence expecting = do
      consumed <- consume
      current <- gets current
      case consumed of
        Nothing ->
          if expecting
            then throwE "Expected another element of sequence"
            else
              if trailing && current == separator
                then do advance; return sequence
                else return sequence
        Just x ->
          let sequence' = sequence ++ [x]
           in if current == separator
                then do
                  advance
                  parseSeparatedSequence' sequence' (not trailing)
                else return sequence'

parseNamespacedIdentifier :: ParserM NamespacedIdentifier
parseNamespacedIdentifier = do
  pieces <- parseSeparatedSequence SeparatorConfig {trailing = False, separator = Glyph "::", consume = consumeSymbol}
  return $ A.NamespacedIdentifier pieces
  where
    consumeSymbol = do
      current <- gets current
      case current of
        Symbol sym -> do advance; return (Just sym)
        _ -> return Nothing
