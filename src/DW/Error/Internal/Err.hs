module DW.Error.Internal.Err where

import Data.Text (Text)
import Text.Printf

data Span = Span Int Int
  deriving (Eq, Show)

data ErrorKind
  = UnrecognizedCharacter Char
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
  | TopLevelBoxedType
  | NonConstTopLevelExpression
  | MultipleDefinitionsOfTLVariable Text
  | DuplicateParameterNames Text
  | NoEntryPointDefined
  | CannotAllocateReference
  | NonConstructibleType
  | WrongConstructorCount {expectedCount :: Int, gotCount :: Int}
  | DereferencedValueType {gotType :: Text}
  | NonDereferenceableType {gotType :: Text}
  deriving (Eq)

data Err = Err ErrorKind Span
  deriving (Eq, Show)

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
  show (OperatorSupport operator ty) = printf "Operator %s does not support type %s" operator ty
  show (UnboundVariable name) = printf "Referenced unbound variable %s" name
  show (CallingNonFunction ty) = printf "Tried to invoke something that's not a function (of type %s)" ty
  show (WrongArgumentCount expected got) = printf "Expected %d arguments, but received %d" expected got
  show BreakOutsideLoop = "Cannot use a break statement outside of a loop"
  show (InvalidBuiltinName name) = printf "Invalid builtin name '%s'" name
  show TopLevelBoxedType = printf "Top-level variables cannot have type 'any'"
  show NonConstTopLevelExpression = printf "Top-level expressions must be constant"
  show (MultipleDefinitionsOfTLVariable name) = printf "Found multiple definitions of %s" name
  show NoEntryPointDefined = printf "No 'main' function was defined"
  show (DuplicateParameterNames name) = printf "Duplicate parameter names: %s and %s" name name
  show CannotAllocateReference = printf "Cannot allocate reference type (only constructible value types can be allocated)"
  show NonConstructibleType = printf "Type is not constructible (cannot be allocated with 'new')"
  show (WrongConstructorCount expected got) = printf "Type constructor expects %d arguments (got %d)" expected got
  show (DereferencedValueType ty) = printf "Can't dereference value type (%s)" (show ty)
  show (NonDereferenceableType ty) = printf "Type %s is non-dereferenceable" (show ty)
