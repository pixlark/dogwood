{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module DW.Error
  ( ErrorKind (..),
    Span (..),
    Err (..),
    Result,
    Result',
    displayError,
    displayErrorColorless,
    isErrorKind,
    isErrorKind',
    throwSpan',
    orThrowSpan',
    orThrowSpanM',
    throwSpan,
    orThrowSpan,
    orThrowSpanM,
    getLineForSpan,
    throwErr,
    markErr,
    runErrorAsErrors,
    throwErrWithCallStack,
    runErrors,
    runErrorsNoCallStack,
    tryErr,
    throwErrs,
    Errors,
  ) where

import DW.Error.Internal
import DW.Error.Internal.ErrorsEffect
