{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Internal where

import AST (AST (..))
import qualified AST as A
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Error
import Lexer
import Text.Printf

data Parser = Parser {current :: Token, lexer :: Lexer, lastTokenEnd :: Int}
  deriving (Show)

type ParserM a = ExceptT ParseError (State Parser) a

advance :: ParserM ()
advance = ExceptT $ state $ \parser ->
  let (Token _ (Span lastTokenStart lastTokenLength)) = parser.current
      lastTokenEnd = lastTokenStart + lastTokenLength
   in case runState nextToken parser.lexer of
        (Right tok, lexer') -> (Right (), parser {current = tok, lexer = lexer', lastTokenEnd})
        (Left e, lexer') -> (Left e, parser {lexer = lexer', lastTokenEnd})

makeParser :: Lexer -> Result Parser
makeParser lexer = parser' <$ result
  where
    parser = Parser {current = Token {kind = Eof, span = Span 0 0}, lexer, lastTokenEnd = 0}
    -- "prime the pump"
    (result, parser') = runState (runExceptT advance) parser

expectKeyword :: Text -> ParserM ()
expectKeyword keyword = do
  current <- gets current
  if current.kind == Keyword keyword
    then advance
    else throwE $ ExpectedKeyword keyword

expectGlyph :: Text -> ParserM ()
expectGlyph glyph = do
  current <- gets current
  if current.kind == Glyph glyph
    then advance
    else throwE $ ExpectedGlyph glyph

readSymbol :: ParserM Text
readSymbol = do
  current <- gets current
  case current.kind of
    Symbol sym -> do advance; return sym
    _ -> throwE ExpectedSymbol

matchKeyword :: Text -> ParserM Bool
matchKeyword keyword = do
  current <- gets current
  if current.kind == Keyword keyword
    then do advance; return True
    else return False

matchGlyph :: Text -> ParserM Bool
matchGlyph glyph = do
  current <- gets current
  if current.kind == Glyph glyph
    then do advance; return True
    else return False

spanStart :: ParserM Int
spanStart = do
  (Token _ (Span start _)) <- gets current
  return start

makeSpan :: Int -> ParserM Span
makeSpan start = do
  end <- gets lastTokenEnd
  return $ Span start (end - start)

data SeparatorConfig a = SeparatorConfig
  { trailing :: Bool,
    separator :: TokenKind,
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
            then throwE ExpectedAnotherElementOfSequence
            else
              if trailing && current.kind == separator
                then do advance; return sequence
                else return sequence
        Just x ->
          let sequence' = sequence ++ [x]
           in if current.kind == separator
                then do
                  advance
                  parseSeparatedSequence' sequence' (not trailing)
                else return sequence'

parseNamespacedIdentifier :: ParserM A.ValueTypeExpr
parseNamespacedIdentifier = do
  pieces <- parseSeparatedSequence SeparatorConfig {trailing = False, separator = Glyph "::", consume = consumeSymbol}
  return $ A.NamespacedIdentifier pieces
  where
    consumeSymbol = do
      current <- gets current
      case current.kind of
        Symbol sym -> do advance; return (Just sym)
        _ -> return Nothing

parseTypeExpr :: ParserM (AST A.TypeExpr)
parseTypeExpr = do
  spanStart <- spanStart
  reference <- matchGlyph "&"
  current <- gets current
  valueExpr <- case current.kind of
    Keyword "void" -> do advance; return A.Void
    Keyword "bool" -> do advance; return A.Bool
    Keyword "int" -> do advance; return A.Int
    Symbol _ -> parseNamespacedIdentifier
    _ -> throwE ExpectedTypeExpr
  span <- makeSpan spanStart
  return $ AST (A.TypeExpr {reference, valueExpr}) span

parseExpr :: ParserM (AST A.Expr)
parseExpr = do
  undefined
