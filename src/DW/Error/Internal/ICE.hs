module DW.Error.Internal.ICE (InternalCompilerError, throwICE, unwrapICE) where

import Control.Exception (Exception, throw)

data InternalCompilerError = InternalCompilerError
  deriving (Show)

throwICE = throw InternalCompilerError

unwrapICE :: Maybe a -> a
unwrapICE (Just x) = x
unwrapICE Nothing = throwICE

instance Exception InternalCompilerError
