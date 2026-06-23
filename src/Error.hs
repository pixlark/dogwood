{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Error (ErrorKind (..), Span (..), Err (..), Result, displayError, isErrorKind) where

import Control.Monad
import Control.Monad.Writer
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Text.Printf
import Util
import Prelude hiding (getLine)

data ErrorKind
  = InternalCompilerError
  | UnrecognizedCharacter Char
  | ExpectedKeyword Text
  | ExpectedGlyph Text
  | ExpectedSymbol
  | ExpectedAnotherElementOfSequence
  | ExpectedTypeExpr
  | ExpectedExpr
  | ExpectedStatement
  | TypeMismatch {expectedType :: Text, gotType :: Text}
  | OperatorSupport {operator :: Text, ty :: Text}
  | UnboundVariable Text
  | CallingNonFunction Text
  | WrongArgumentCount {expectedCount :: Int, gotCount :: Int}
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

getLine :: Text -> Int -> Text
getLine source 1 = T.take lineLength source
  where
    lineLength = findIndex source 0 '\n' `orElse` T.length source
getLine "" n | n > 1 = error "bad line number"
getLine source n | n > 1 = getLine (T.drop nextLine source) (n - 1)
  where
    nextLine = ((+ 1) <$> findIndex source 0 '\n') `orElse` T.length source
getLine _ _ = error "bad line number"

getLineNumber :: Text -> Int -> Int
getLineNumber text start = getLineNumber' text start 1
  where
    getLineNumber' text start acc =
      if
        | start < 0 -> acc
        | text `T.index` start == '\n' -> getLineNumber' text (start - 1) (acc + 1)
        | otherwise -> getLineNumber' text (start - 1) acc

tellRed :: (MonadWriter Text m) => Text -> m ()
tellRed text = do
  tell "\x1b[1;31m"
  tell text
  tell "\x1b[0m"

displayError :: Text -> Err -> Text
displayError source (Err error span) = execWriter $ do
  tell "\n--------------- ERROR ---------------\n"

  let (excerpt, Span start len) = getLineForSpan source span
  let lineNumber = getLineNumber source start
  let totalLines = getLineNumber source (T.length source - 1)
  let preContextStart = max (lineNumber - 3) 1
  let postContextEnd = min (lineNumber + 3) totalLines

  tell $ T.show error
  tell "\n\n"

  forM_ [preContextStart .. lineNumber - 1] $ \line -> do
    tell " "
    tell $ T.show line
    tell "  "
    tell $ getLine source line
    tell "\n"

  tell " "
  tell $ T.show lineNumber
  tell "  "
  tell excerpt
  tell "\n "
  let digits = length $ show lineNumber
  forM_ (replicate (start + digits) " ") tell
  tell "  "
  forM_ (replicate len "~") tellRed
  tell "\n"

  forM_ [lineNumber + 1 .. postContextEnd] $ \line -> do
    tell " "
    tell $ T.show line
    tell "  "
    tell $ getLine source line
    tell "\n"

  tell "\n-------------------------------------\n"

instance Show ErrorKind where
  show InternalCompilerError = "Internal compiler error!"
  show (UnrecognizedCharacter c) = printf "Unrecognized character %c" c
  show (ExpectedKeyword keyword) = printf "Expected keyword %s" keyword
  show (ExpectedGlyph glyph) = printf "Expected glyph %s" glyph
  show ExpectedSymbol = "Expected symbol"
  show ExpectedAnotherElementOfSequence = "Expected another element of sequence"
  show ExpectedTypeExpr = "Expected type expression"
  show ExpectedExpr = "Expected expression"
  show ExpectedStatement = "Expected statement"
  show (TypeMismatch expected got) = printf "Expected type of %s, but got %s" expected got
  show (OperatorSupport operator ty) = printf "Operator %s does not support type %s" operator ty
  show (UnboundVariable name) = printf "Referenced unbound variable %s" name
  show (CallingNonFunction ty) = printf "Tried to invoke something that's not a function (of type %s)" ty
  show (WrongArgumentCount expected got) = printf "Expected %d arguments, but received %d" expected got

type Result a = Either Err a

isErrorKind :: ErrorKind -> (Result a -> Bool)
isErrorKind kind = \case
  Left (Err kind' _) -> kind == kind'
  Right _ -> False
