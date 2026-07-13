{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module DW.Error
  ( ErrorKind (..),
    Span (..),
    Err (..),
    Result,
    displayError,
    displayErrorColorless,
    isErrorKind,
    throwSpan,
    markSpan,
    orThrowSpan,
    orThrowSpanM,
    orElseMarkSpan,
    orElseMarkSpanM,
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
    InternalCompilerError,
    throwICE,
    unwrapICE,
  )
where

import DW.Error.Internal
import DW.Error.Internal.Display
import DW.Error.Internal.Err
import DW.Error.Internal.ErrorsEffect
import DW.Error.Internal.ICE
