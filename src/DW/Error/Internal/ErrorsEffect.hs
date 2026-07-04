{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DW.Error.Internal.ErrorsEffect where

-- import Effectful.Dispatch.Dynamic

import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Effectful
import Effectful.Dispatch.Static
import Effectful.Exception
import Effectful.Internal.Utils
import GHC.Stack

data Errors e :: Effect
type instance DispatchOf (Errors e) = Static NoSideEffects

data ErrorsState e = ErrorsState
  { errorsRef :: IORef [e],
    errorId :: ErrorId
  }

newtype instance StaticRep (Errors e) = Errors (ErrorsState e)

runErrors :: Eff (Errors e : es) a -> Eff es (Either [e] a)
runErrors action = do
  ref <- unsafeEff_ $ newIORef []
  eid <- unsafeEff_ newErrorId
  result <-
    evalStaticRep (Errors (ErrorsState ref eid)) $
      tryJust (matchError eid) action
  errors <- unsafeEff_ $ readIORef ref
  return $ case errors of
    [] -> case result of
      Right result -> Right result
      Left _ -> error "unreachable"
    es -> Left (reverse es)

throwErr :: (HasCallStack, Show e, Errors e :> es) => e -> Eff es a
throwErr e = do
  Errors (ErrorsState ref eid) <- getStaticRep
  _ <- unsafeEff_ $ modifyIORef ref (e :)
  withFrozenCallStack throwIO $ ErrorWrapper eid emptyCallStack (show e) (toAny e)

markErr :: (HasCallStack, Errors e :> es) => e -> Eff es ()
markErr e = do
  Errors (ErrorsState ref _) <- getStaticRep
  unsafeEff_ $ modifyIORef ref (e :)

--
-- The following is taken directly from effectful's implementation of Effectul.Error.Static
--

newtype ErrorId = ErrorId Unique
  deriving newtype (Eq)

-- | A unique is picked so that distinct 'Error' handlers for the same type
-- don't catch each other's exceptions.
newErrorId :: IO ErrorId
newErrorId = ErrorId <$> newUnique

data ErrorWrapper = ErrorWrapper !ErrorId CallStack String Any

instance Show ErrorWrapper where
  showsPrec :: Int -> ErrorWrapper -> ShowS
  showsPrec _ (ErrorWrapper _ cs errRep _) =
    ("Effectful.Error.Static.ErrorWrapper: " ++)
      . (errRep ++)
      . ("\n" ++)
      . (prettyCallStack cs ++)

instance Exception ErrorWrapper where
  -- See discussion in https://github.com/haskell-effectful/effectful/pull/232.
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

matchError :: ErrorId -> ErrorWrapper -> Maybe (CallStack, e)
matchError eid (ErrorWrapper etag cs _ e)
  | eid == etag = Just (cs, fromAny e)
  | otherwise = Nothing
