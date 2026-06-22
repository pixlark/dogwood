{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lexer.Internal where

import Control.Monad
-- import Control.Monad.Except
-- import Control.Monad.State.Lazy
-- import Control.Monad.Trans.Except
-- import Control.Monad.Trans.Maybe
import Data.Char
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.State.Static.Local (State, gets, modify)
import Error
import Text.Printf
import Util

data TokenKind = Eof | Symbol Text | Keyword Text | Glyph Text | IntLiteral Int
  deriving (Eq, Show)

data Token = Token {kind :: TokenKind, span :: Span}
  deriving (Eq, Show)

data Lexer = Lexer {cursor :: Int, source :: Text}
  deriving (Show)

type LexerE a = Eff '[State Lexer, Error Err] a

advance :: (State Lexer :> es) => Eff es ()
advance = modify advance'
  where
    advance' lexer = lexer {cursor = lexer.cursor + 1}

advanceBy :: (State Lexer :> es) => Int -> Eff es ()
advanceBy n = modify $ advanceBy' n
  where
    advanceBy' n lexer = lexer {cursor = lexer.cursor + n}

current :: (State Lexer :> es) => Eff es (Maybe Char)
current = gets current'
  where
    current' :: Lexer -> Maybe Char
    current' lexer =
      if lexer.cursor >= T.length lexer.source
        then Nothing
        else Just $ T.head $ T.drop lexer.cursor lexer.source

makeLexer :: T.Text -> Lexer
makeLexer source = Lexer {cursor = 0, source}

keywords :: [T.Text]
keywords =
  [ "void",
    "arr",
    "bool",
    "int",
    "new",
    "cast",
    "if",
    "else",
    "switch",
    "let",
    "return",
    "loop",
    "fn",
    "where",
    "struct",
    "union",
    "enum",
    "typeclass",
    "instance",
    "true",
    "false",
    "break"
  ]

validGlyphs :: [T.Text]
validGlyphs =
  [ "[",
    "]",
    "{",
    "}",
    "(",
    ")",
    "+",
    "-",
    "*",
    "/",
    "=",
    "==",
    "!=",
    ">",
    "<",
    ">=",
    "<=",
    "&",
    "&&",
    "||",
    "!",
    "->",
    "=>",
    ",",
    ",",
    ",",
    ":",
    "::",
    ";"
  ]

validGlyphStarts :: [Char]
validGlyphStarts = List.nub $ map T.head validGlyphs

tryMakeSingleGlyph :: (State Lexer :> es) => Char -> Eff es (Maybe Token)
tryMakeSingleGlyph c =
  if s `elem` validGlyphs
    then do
      token <- makeToken 1 (Glyph s)
      return $ Just token
    else return Nothing
  where
    s = T.pack [c]

tryMakeDoubleGlyph :: (State Lexer :> es) => Char -> Char -> Eff es (Maybe Token)
tryMakeDoubleGlyph c1 c2 =
  if s `elem` validGlyphs
    then do
      token <- makeToken 2 (Glyph s)
      return $ Just token
    else return Nothing
  where
    s = T.pack [c1, c2]

makeToken :: (State Lexer :> es) => Int -> TokenKind -> Eff es Token
makeToken length kind = do
  cursor <- gets cursor
  return $ Token kind $ Span (cursor - length) length

nextToken :: (State Lexer :> es, Error Err :> es) => Eff es Token
nextToken = do
  skipWhitespace
  c <- current
  cur <- gets cursor
  case c of
    Nothing -> makeToken 0 Eof
    Just c ->
      let unrecognizedCharacter = Err (UnrecognizedCharacter c) (Span cur 1)
       in if
            | isDigit c -> do
                (src, cur) <- gets ((,) <$> source <*> cursor)
                let numberString = T.takeWhile isDigit $ T.drop cur src
                let length = T.length numberString
                advanceBy length
                makeToken length $ IntLiteral $ read $ T.unpack numberString
            | isAlpha c || c == '_' -> do
                (src, cur) <- gets ((,) <$> source <*> cursor)
                let symbol = T.takeWhile (\c -> isAlpha c || c == '_') $ T.drop cur src
                let length = T.length symbol
                advanceBy length
                if symbol `elem` keywords
                  then makeToken length $ Keyword symbol
                  else makeToken length $ Symbol symbol
            | c `elem` validGlyphStarts -> do
                advance
                c' <- current
                case c' of
                  {- HLINT ignore -}
                  Nothing -> eitherFromMaybe unrecognizedCharacter <$> tryMakeSingleGlyph c >>= except
                  Just c' -> eitherFromMaybe unrecognizedCharacter <$> tryMakeDoubleOrSingleGlyph c c' >>= except
            | otherwise -> throwError unrecognizedCharacter
  where
    skipWhitespace = do
      c <- current
      when (c == Just ' ' || c == Just '\n') (do advance; skipWhitespace)
    tryMakeDoubleOrSingleGlyph c c' = do
      doubleGlyph <- tryMakeDoubleGlyph c c'
      case doubleGlyph of
        Nothing -> tryMakeSingleGlyph c
        j@(Just _) -> do advance; return j
    except (Right value) = return value
    except (Left e) = throwError e
