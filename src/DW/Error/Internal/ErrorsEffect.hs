{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DW.Error.Internal.ErrorsEffect where

-- import Effectful.Dispatch.Dynamic

import Control.Monad (forM_, void)
import Data.Bifunctor (Bifunctor (..))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Effectful
import Effectful.Dispatch.Static
import qualified Effectful.Error.Static as E
import Effectful.Exception
import Effectful.Internal.Utils
import GHC.Stack

data Errors e :: Effect
type instance DispatchOf (Errors e) = Static NoSideEffects

data ErrorsState e = ErrorsState
  { errorsRef :: IORef [(CallStack, e)],
    errorId :: ErrorId,
    abortErrorId :: ErrorId
  }

newtype instance StaticRep (Errors e) = Errors (ErrorsState e)

runErrors :: (HasCallStack) => Eff (Errors e : es) a -> Eff es (Either [(CallStack, e)] a)
runErrors action = do
  ref <- unsafeEff_ $ newIORef []
  eid <- unsafeEff_ newErrorId
  aid <- unsafeEff_ newErrorId
  result <-
    evalStaticRep (Errors (ErrorsState ref eid aid)) $
      tryIf (isMatch . matchError eid aid) action
  errors <- unsafeEff_ $ readIORef ref
  return $ case errors of
    [] -> case result of
      Right result -> Right result
      Left _ -> error "unreachable"
    es -> Left (reverse es)

runErrorsNoCallStack :: (HasCallStack) => Eff (Errors e : es) a -> Eff es (Either [e] a)
runErrorsNoCallStack action = do
  result <- runErrors action
  return $ map snd `first` result

throwErr :: (Show e, HasCallStack, Errors e :> es) => e -> Eff es a
throwErr e = do
  Errors (ErrorsState ref eid _) <- getStaticRep
  _ <- unsafeEff_ $ modifyIORef ref ((callStack, e) :)
  withFrozenCallStack throwIO $ ErrorWrapper eid callStack (show e) (toAny e)

throwErrs :: (Show e, HasCallStack, Errors e :> es) => [e] -> Eff es a
throwErrs errs = do
  forM_ (take (length errs - 1) errs) $ \e ->
    markErr e
  throwErr (last errs)

abortIfAnyErrors :: (Show e, HasCallStack, Errors e :> es) => Eff es ()
abortIfAnyErrors = do
  Errors (ErrorsState ref _ aid) <- getStaticRep
  errors <- unsafeEff_ $ readIORef ref
  case errors of
    [] -> return ()
    _ -> withFrozenCallStack throwIO $ ErrorWrapper aid emptyCallStack "" (toAny ())

throwErrWithCallStack :: (Show e, HasCallStack, Errors e :> es) => CallStack -> e -> Eff es a
throwErrWithCallStack cs e = do
  Errors (ErrorsState ref eid _) <- getStaticRep
  _ <- unsafeEff_ $ modifyIORef ref ((cs, e) :)
  withFrozenCallStack throwIO $ ErrorWrapper eid callStack (show e) (toAny e)

throwErrsWithCallStacks :: (Show e, HasCallStack, Errors e :> es) => [(CallStack, e)] -> Eff es a
throwErrsWithCallStacks errs = do
  Errors (ErrorsState ref eid _) <- getStaticRep
  _ <- unsafeEff_ $ modifyIORef ref (errs ++)
  withFrozenCallStack throwIO $ ErrorWrapper eid callStack (show (last errs)) (toAny (last errs))

markErr :: (HasCallStack, Errors e :> es) => e -> Eff es ()
markErr e = do
  Errors (ErrorsState ref _ _) <- getStaticRep
  unsafeEff_ $ modifyIORef ref ((callStack, e) :)

runErrorAsErrors :: (HasCallStack, Show e, Errors e :> es) => Eff (E.Error e : es) a -> Eff es a
runErrorAsErrors action = do
  result <- E.runError action
  case result of
    Left (cs, e) -> throwErrWithCallStack cs e
    Right x -> return x

tryErr :: (Show e, HasCallStack, Errors e :> es) => Eff es a -> Eff es (Either [(CallStack, e)] a)
tryErr action = do
  Errors (ErrorsState ref eid aid) <- getStaticRep
  catchIf (isMatch . matchError eid aid) (Right <$> action) $ \_ -> do
    errors <- unsafeEff_ $ readIORef ref
    return $ Left errors

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

data ErrorMatch e = NoMatch | AbortMatch | ErrorMatch (CallStack, e)

isMatch :: ErrorMatch e -> Bool
isMatch NoMatch = False
isMatch AbortMatch = True
isMatch (ErrorMatch _) = True

matchError :: ErrorId -> ErrorId -> ErrorWrapper -> ErrorMatch e
matchError eid aid (ErrorWrapper etag cs _ e)
  | eid == etag = ErrorMatch (cs, fromAny e)
  | aid == etag = AbortMatch
  | otherwise = NoMatch
