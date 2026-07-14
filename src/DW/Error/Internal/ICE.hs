module DW.Error.Internal.ICE (InternalCompilerError (..), throwICE, unwrapICE) where

import Control.Exception (Exception, throw)
import GHC.Stack (CallStack, HasCallStack, callStack)

newtype InternalCompilerError = InternalCompilerError CallStack
  deriving (Show)

throwICE :: (HasCallStack) => a
throwICE = throw (InternalCompilerError callStack)

unwrapICE :: (HasCallStack) => Maybe a -> a
unwrapICE (Just x) = x
unwrapICE Nothing = throwICE

instance Exception InternalCompilerError
