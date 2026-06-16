{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Internal where

import AST (AST (..))
import qualified AST as A
import Control.Monad
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
   in case runState (runExceptT nextToken) parser.lexer of
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

data MaybeSpanned a = AlreadySpanned (AST a) | NotSpanned a

direct :: AST a -> MaybeSpanned a
direct = AlreadySpanned

wrap :: a -> MaybeSpanned a
wrap = NotSpanned

returnDirect :: AST a -> ParserM (MaybeSpanned a)
returnDirect = return . direct

returnWrap :: a -> ParserM (MaybeSpanned a)
returnWrap = return . wrap

produceSpannedAST :: ParserM (MaybeSpanned a) -> ParserM (AST a)
produceSpannedAST f = do
  spanStart <- spanStart
  value <- f
  span <- makeSpan spanStart
  case value of
    AlreadySpanned x -> return x
    NotSpanned x -> return $ AST x span

parseTypeExpr :: ParserM (AST A.TypeExpr)
parseTypeExpr = produceSpannedAST $ do
  reference <- matchGlyph "&"
  current <- gets current
  valueExpr <- case current.kind of
    Keyword "void" -> do advance; return A.Void
    Keyword "bool" -> do advance; return A.Bool
    Keyword "int" -> do advance; return A.Int
    Symbol _ -> parseNamespacedIdentifier
    _ -> throwE ExpectedTypeExpr
  return $ NotSpanned A.TypeExpr {reference, valueExpr}

parseAtom :: ParserM (AST A.Expr)
parseAtom = produceSpannedAST $ do
  current <- gets current
  case current.kind of
    Keyword "void" -> do advance; returnWrap A.VoidLit
    Keyword "true" -> do advance; returnWrap (A.BoolLit True)
    Keyword "false" -> do advance; returnWrap (A.BoolLit False)
    IntLiteral n -> do advance; returnWrap (A.IntLit n)
    Glyph "(" -> do
      advance
      (AST expr _) <- parseExpr
      expectGlyph ")"
      returnWrap expr
    _ -> throwE ExpectedExpr

parseBinary :: ParserM (AST A.Expr) -> [(TokenKind, AST A.Expr -> AST A.Expr -> A.Expr)] -> ParserM (AST A.Expr)
parseBinary nextParser operators = produceSpannedAST $ do
  left <- nextParser
  current <- gets current
  results <- forM operators $ \(operator, combiner) ->
    if current.kind == operator
      {- HLINT ignore -}
      then do
        advance
        right <- recurse
        return $ Just $ wrap $ combiner left right
      else return Nothing
  case msum results of
    Just operator -> return operator
    Nothing -> returnDirect left
  where
    recurse = parseBinary nextParser operators

parseBinaryMultiplicative :: ParserM (AST A.Expr)
parseBinaryMultiplicative =
  parseBinary
    parseAtom
    [ (Glyph "*", A.Operator A.Multiply),
      (Glyph "/", A.Operator A.Divide)
    ]

parseBinaryAdditive :: ParserM (AST A.Expr)
parseBinaryAdditive =
  parseBinary
    parseBinaryMultiplicative
    [ (Glyph "+", A.Operator A.Plus),
      (Glyph "-", A.Operator A.Minus)
    ]

parseBinaryComparison :: ParserM (AST A.Expr)
parseBinaryComparison =
  parseBinary
    parseBinaryAdditive
    [ (Glyph "<", A.Operator A.LessThan),
      (Glyph "<=", A.Operator A.LessThanOrEqual),
      (Glyph ">", A.Operator A.GreaterThan),
      (Glyph ">=", A.Operator A.GreaterThanOrEqual)
    ]

parseBinaryEquality :: ParserM (AST A.Expr)
parseBinaryEquality =
  parseBinary
    parseBinaryComparison
    [ (Glyph "==", A.Operator A.Equal),
      (Glyph "!=", A.Operator A.NotEqual)
    ]

parseBinaryAnd :: ParserM (AST A.Expr)
parseBinaryAnd =
  parseBinary
    parseBinaryEquality
    [ (Glyph "&&", A.Operator A.And)
    ]

parseBinaryOr :: ParserM (AST A.Expr)
parseBinaryOr =
  parseBinary
    parseBinaryAnd
    [ (Glyph "||", A.Operator A.Or)
    ]

parseExpr :: ParserM (AST A.Expr)
parseExpr = parseBinaryOr
