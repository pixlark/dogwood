module DW.Error.Internal where

import DW.Error.Internal.Err
import DW.Error.Internal.ErrorsEffect
import DW.Util

import Control.Monad
import Effectful (Eff, (:>))
import Effectful.Error.Static (HasCallStack)

type Result a = Either [Err] a

isErrorKind :: ErrorKind -> (Result a -> Bool)
isErrorKind kind = \case
  Left [Err kind' _] -> kind == kind'
  Left _ -> False
  Right _ -> False

throwSpan :: (HasCallStack, Errors Err :> es) => Span -> ErrorKind -> Eff es a
throwSpan span kind = throwErr $ Err kind span

orThrowSpan :: (HasCallStack, Errors Err :> es) => Maybe a -> (Span, ErrorKind) -> Eff es a
orThrowSpan m (span, kind) = maybe (throwSpan span kind) return m

orThrowSpanM :: (HasCallStack, Errors Err :> es) => Eff es (Maybe a) -> (Span, ErrorKind) -> Eff es a
orThrowSpanM m e = do
  m' <- m
  orThrowSpan m' e

-- | Unpack the `Maybe` value, or otherwise throw an internal compiler error with the given span
orICE :: (HasCallStack, Errors Err :> es) => Maybe a -> Span -> Eff es a
orICE m span = m `orThrowSpan` (span, InternalCompilerError)

-- | Unpack the `Maybe` value produced by the effect, or otherwise throw an internal compiler error with the given span
orICEM :: (HasCallStack, Errors Err :> es) => Eff es (Maybe a) -> Span -> Eff es a
orICEM m span = m `orThrowSpanM` (span, InternalCompilerError)

markSpan :: (HasCallStack, Errors Err :> es) => Span -> ErrorKind -> Eff es ()
markSpan span kind = markErr $ Err kind span

orElseMarkSpan :: (HasCallStack, Errors Err :> es) => Maybe a -> (Span, ErrorKind, a) -> Eff es a
orElseMarkSpan m (span, kind, deflt) = do
  when (null m) $ markSpan span kind
  return $ m `orElse` deflt

orElseMarkSpanM :: (HasCallStack, Errors Err :> es) => Eff es (Maybe a) -> (Span, ErrorKind, a) -> Eff es a
orElseMarkSpanM m (span, kind, deflt) = do
  m' <- m
  when (null m') $ markSpan span kind
  return $ m' `orElse` deflt
