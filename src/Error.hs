module Error where

import Data.Text (Text)
import Text.Printf

data ParseError
  = UnrecognizedCharacter Char
  | ExpectedKeyword Text
  | ExpectedGlyph Text
  | ExpectedSymbol
  | ExpectedBuiltinType
  | ExpectedAnotherElementOfSequence
  deriving (Eq)

instance Show ParseError where
  show (UnrecognizedCharacter c) = printf "Unrecognized character %c" c
  show (ExpectedKeyword keyword) = printf "Expected keyword %s" keyword
  show (ExpectedGlyph glyph) = printf "Expected glyph %s" glyph
  show ExpectedSymbol = "Expected symbol"
  show ExpectedBuiltinType = "Expected builtin type (void, bool, int, etc)"
  show ExpectedAnotherElementOfSequence = "Expected another element of sequence"

type Result a = Either ParseError a
