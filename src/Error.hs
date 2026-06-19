{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Error (ErrorKind (..), Span (..), Err (..), Result, displayError) where

import Control.Monad
import Control.Monad.Writer
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Text.Printf
import Util

data ErrorKind
  = UnrecognizedCharacter Char
  | ExpectedKeyword Text
  | ExpectedGlyph Text
  | ExpectedSymbol
  | ExpectedAnotherElementOfSequence
  | ExpectedTypeExpr
  | ExpectedExpr
  | ExpectedStatement
  | TypeMismatch {expected :: Text, got :: Text}
  | InternalCompilerError
  deriving (Eq)

data Span = Span Int Int
  deriving (Eq, Show)

data Err = Err ErrorKind Span
  deriving (Eq, Show)

findIndex :: Text -> Int -> Char -> Maybe Int
findIndex text start c =
  if
    | start >= T.length text -> Nothing
    | text `T.index` start == c -> Just start
    | otherwise -> findIndex text (start + 1) c

findIndexRev :: Text -> Int -> Char -> Maybe Int
findIndexRev text start c =
  if
    | start < 0 -> Nothing
    | text `T.index` start == c -> Just start
    | otherwise -> findIndexRev text (start - 1) c

getLineForSpan :: Text -> Span -> (Text, Span)
getLineForSpan source (Span start len) = (line, span')
  where
    lineStart = ((+ 1) <$> findIndexRev source start '\n') `orElse` 0
    lineEnd = findIndex source (start + len) '\n' `orElse` T.length source
    lineLength = lineEnd - lineStart
    line = T.take lineLength $ T.drop lineStart source
    span' = Span (start - lineStart) len

getLineNumber :: Text -> Int -> Int
getLineNumber text start = getLineNumber' text start 1
  where
    getLineNumber' text start acc =
      if
        | start < 0 -> acc
        | text `T.index` start == '\n' -> getLineNumber' text (start - 1) (acc + 1)
        | otherwise -> getLineNumber' text (start - 1) acc

displayError :: Text -> Err -> Text
displayError source (Err error span) = execWriter $ do
  let (excerpt, Span start len) = getLineForSpan source span
  let lineNumber = getLineNumber source start
  let digits = length $ show lineNumber
  tell $ T.show error
  tell "\n "
  tell $ T.show lineNumber
  tell "  "
  tell excerpt
  tell "\n "
  forM_ (replicate (start + digits) " ") tell
  tell "  "
  forM_ (replicate len "^") tell
  tell "\n"

instance Show ErrorKind where
  show (UnrecognizedCharacter c) = printf "Unrecognized character %c" c
  show (ExpectedKeyword keyword) = printf "Expected keyword %s" keyword
  show (ExpectedGlyph glyph) = printf "Expected glyph %s" glyph
  show ExpectedSymbol = "Expected symbol"
  show ExpectedAnotherElementOfSequence = "Expected another element of sequence"
  show ExpectedTypeExpr = "Expected type expression"
  show ExpectedExpr = "Expected expression"
  show ExpectedStatement = "Expected statement"
  show (TypeMismatch expected got) = printf "Expected type of %s, but got %s" expected got
  show InternalCompilerError = "Internal compiler error!"

type Result a = Either Err a
