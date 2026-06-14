{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal where

import qualified AST as A
import Control.Monad.State.Lazy
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Lexer
import Text.Printf

data Parser = Parser {current :: Token, lexer :: Lexer}
  deriving (Show)

type ParserM a = State Parser a

type Result a = Either String a

advance :: ParserM (Result ())
advance = state $ \parser -> case runState nextToken parser.lexer of
  (Right tok, lexer') -> (Right (), parser {current = tok, lexer = lexer'})
  (Left e, lexer') -> (Left e, parser {lexer = lexer'})

makeParser :: Lexer -> Result Parser
makeParser lexer = parser' <$ result
  where
    parser = Parser {current = Eof, lexer}
    -- "prime the pump"
    (result, parser') = runState advance parser

expectKeyword :: Text -> ParserM (Result ())
expectKeyword keyword = do
  current <- gets current
  if current == Keyword keyword
    then advance
    else return $ Left $ printf "Expected keyword %s" keyword

expectGlyph :: Text -> ParserM (Result ())
expectGlyph glyph = do
  current <- gets current
  if current == Glyph glyph
    then advance
    else return $ Left $ printf "Expected glyph %s" glyph

readSymbol :: ParserM (Result Text)
readSymbol = do
  current <- gets current
  case current of
    Symbol sym -> advance <&> (sym <$)
    _ -> return $ Left "Expected symbol"

matchKeyword :: Text -> ParserM (Result Bool)
matchKeyword keyword = do
  current <- gets current
  if current == Keyword keyword
    then advance <&> (True <$)
    else return $ Right False

matchGlyph :: Text -> ParserM (Result Bool)
matchGlyph glyph = do
  current <- gets current
  if current == Glyph glyph
    then advance <&> (True <$)
    else return $ Right False

parseBuiltinType :: ParserM (Result A.BuiltinType)
parseBuiltinType = do
  current <- gets current
  return $ case current of
    Keyword "void" -> Right A.Void
    Keyword "bool" -> Right A.Bool
    Keyword "int" -> Right A.Int
    _ -> Left "Expected builtin type"
