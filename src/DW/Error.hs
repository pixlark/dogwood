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
    orThrowSpan,
    orThrowSpanM,
    getLineForSpan,
  ) where

import DW.Error.Internal
