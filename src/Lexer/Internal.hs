{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Internal where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Char
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Error
import Text.Printf
import Util

data Token = Eof | Symbol Text | Keyword Text | Glyph Text
  deriving (Eq, Show)

data Lexer = Lexer {cursor :: Int, source :: Text}
  deriving (Show)

type LexerM a = State Lexer a

advance :: LexerM ()
advance = modify advance'
  where
    advance' lexer = lexer {cursor = lexer.cursor + 1}

advanceBy :: Int -> LexerM ()
advanceBy n = modify $ advanceBy' n
  where
    advanceBy' n lexer = lexer {cursor = lexer.cursor + n}

current :: LexerM (Maybe Char)
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
    "instance"
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
    "::"
  ]

validGlyphStarts :: [Char]
validGlyphStarts = List.nub $ map T.head validGlyphs

tryMakeSingleGlyph :: Char -> Maybe Token
tryMakeSingleGlyph c = if s `elem` validGlyphs then Just $ Glyph s else Nothing
  where
    s = T.pack [c]

tryMakeDoubleGlyph :: Char -> Char -> Maybe Token
tryMakeDoubleGlyph c1 c2 = if s `elem` validGlyphs then Just $ Glyph s else Nothing
  where
    s = T.pack [c1, c2]

nextToken :: LexerM (Result Token)
nextToken = do
  skipWhitespace
  c <- current
  case c of
    Nothing -> return $ Right Eof
    Just c ->
      if isAlpha c || c == '_'
        then do
          (src, cur) <- gets ((,) <$> source <*> cursor)
          let symbol = T.takeWhile (\c -> isAlpha c || c == ' ') $ T.drop cur src
          advanceBy $ T.length symbol
          return $
            Right $
              if symbol `elem` keywords
                then Keyword symbol
                else Symbol symbol
        else
          if c `elem` validGlyphStarts
            then do
              advance
              c' <- current
              case c' of
                Nothing -> return $ eitherFromMaybe (UnrecognizedCharacter c) (tryMakeSingleGlyph c)
                Just c' -> eitherFromMaybe (UnrecognizedCharacter c) <$> tryMakeDoubleOrSingleGlyph c c'
            else return $ Left (UnrecognizedCharacter c)
  where
    skipWhitespace = do
      c <- current
      when (c == Just ' ' || c == Just '\n') (advance >> skipWhitespace)
    tryMakeDoubleOrSingleGlyph c c' = case tryMakeDoubleGlyph c c' of
      Nothing -> return $ tryMakeSingleGlyph c
      j@(Just _) -> advance >> return j
