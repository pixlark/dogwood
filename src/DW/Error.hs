{-# LANGUAGE QuasiQuotes #-}

module DW.Error
  ( ErrorKind (..),
    Span (..),
    Err (..),
    Result,
    displayError,
    displayErrorColorless,
    isErrorKind,
    throwSpan,
    orThrowSpan,
    orThrowSpanM,
    getLineForSpan,
  ) where

import Control.Monad
import DW.Util
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Effectful (Eff, runPureEff, (:>))
import Effectful.Error.Static (Error, HasCallStack, throwError)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.Writer.Static.Local (Writer, execWriter, tell)
import Text.Printf
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
  | BreakOutsideLoop
  | InvalidBuiltinName Text
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
    span' = traceShowId $ Span (start - lineStart) len

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

tellRed :: (Writer Text :> es, Reader Bool :> es) => Text -> Eff es ()
tellRed text = do
  useColor <- ask
  when useColor $ tell "\x1b[1;31m"
  tell text
  when useColor $ tell "\x1b[0m"

leftPad :: Int -> Int -> Text
leftPad to n = padded
  where
    unpadded = T.show n
    digits = T.length unpadded
    padLength = max 0 (to - digits)
    padded = T.pack (map (const ' ') [1 .. padLength]) `T.append` unpadded

displayError :: Text -> Err -> Text
displayError = displayError' True

displayErrorColorless :: Text -> Err -> Text
displayErrorColorless = displayError' False

displayError' :: Bool -> Text -> Err -> Text
displayError' useColor source (Err error span@(Span spanStart _)) = runPureEff $ execWriter $ runReader useColor $ do
  do tell "\n--------------- "; tellRed "ERROR"; tell " ---------------\n\n"

  let (excerpt, Span excerptStart excerptLen) = getLineForSpan source span
  let lineNumber = getLineNumber source spanStart
  let totalLines = getLineNumber source (T.length source - 1)
  let preContextStart = max (lineNumber - 3) 1
  let postContextEnd = min (lineNumber + 3) totalLines
  let spansLines = T.length (T.filter (== '\n') excerpt) + 1
  let pad = leftPad 3

  tell $ T.show error
  tell "\n\n"

  forM_ [preContextStart .. lineNumber - 1] $ \line -> do
    tell " "
    tell $ pad line
    tell "   "
    tell $ getLine source line
    tell "\n"

  if spansLines == 1
    then do
      tell " "
      tell $ pad lineNumber
      tell "   "
      tell excerpt
      tell "\n   "
      tellRed "e "
      forM_ (replicate excerptStart " ") tellRed
      tell "  "
      forM_ (replicate excerptLen "^") tellRed
      tell "\n"
    else do
      forM_ [lineNumber .. lineNumber + spansLines - 1] $ \line -> do
        tell " "
        tell $ pad line
        tellRed " > "
        tell $ getLine source line
        tell "\n"

  forM_ [lineNumber + spansLines .. postContextEnd] $ \line -> do
    tell " "
    tell $ pad line
    tell "   "
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
  show BreakOutsideLoop = "Cannot use a break statement outside of a loop"
  show (InvalidBuiltinName name) = printf "Invalid builtin name '%s'" name

type Result a = Either Err a

isErrorKind :: ErrorKind -> (Result a -> Bool)
isErrorKind kind = \case
  Left (Err kind' _) -> kind == kind'
  Right _ -> False

throwSpan :: (HasCallStack, Error Err :> es) => Span -> ErrorKind -> Eff es a
throwSpan span kind = throwError $ Err kind span

orThrowSpan :: (HasCallStack, Error Err :> es) => Maybe a -> (Span, ErrorKind) -> Eff es a
orThrowSpan m (span, kind) = maybe (throwSpan span kind) return m

orThrowSpanM :: (HasCallStack, Error Err :> es) => Eff es (Maybe a) -> (Span, ErrorKind) -> Eff es a
orThrowSpanM m e = do
  m' <- m
  orThrowSpan m' e
