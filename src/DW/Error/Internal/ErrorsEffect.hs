{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module models the `Errors` effect.
--
-- `Errors` is a static effect, very similar to `Effectful.Error.Static` (and in fact implemented basically
-- in the same manner). Unlike the standard `Error` effect, which produces only one error, and aborts computation
-- as soon as that error is produced, `Errors` allows you to mark down an error for later, but attempt to continue
-- computation anyways.
--
-- At the end of computation, if any errors have been marked, then the entire computation fails and produces a list
-- of all the errors. Otherwise, it produces the result as usual.
--
-- This is useful in a compiler, because then the user doesn't have to only see one error at a time.
--
-- ---
--
-- Implementation notes:
--
-- The implementation is modeled after the implementation of `Effectful.Error.Static`.
--
-- Internally, it uses `unsafeEff_` to generate unique IDs and to throw and catch `IO` exceptions.
--
-- This is safe because the use of `IO` here doesn't perform visible side effects, and since the entire effects
-- stack always runs underneath `IO` anyways, we don't have to worry about sequencing or double-evaluation.
module DW.Error.Internal.ErrorsEffect where

import DW.Util (leftMap)

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static qualified as E
import Effectful.Exception
import Effectful.Internal.Utils
import GHC.Stack

data Errors e :: Effect
type instance DispatchOf (Errors e) = Static NoSideEffects

data instance StaticRep (Errors e)
  = Errors
  { errors :: NonEmpty [(CallStack, e)],
    errorId :: ErrorId,
    abortId :: ErrorId
  }

runErrors :: (HasCallStack) => Eff (Errors e : es) a -> Eff es (Either [(CallStack, e)] a)
runErrors action = do
  let errs = NE.singleton []
  eid <- unsafeEff_ newErrorId
  aid <- unsafeEff_ newErrorId
  (result, Errors {errors}) <-
    runStaticRep (Errors errs eid aid) $
      tryIf (isMatch . matchError eid aid) action
  return $ case errors of
    [] :| _ -> case result of
      Right result -> Right result
      Left _ -> error "unreachable"
    es :| _ -> Left (reverse es)

runErrorsNoCallStack :: (HasCallStack) => Eff (Errors e : es) a -> Eff es (Either [e] a)
runErrorsNoCallStack action = do
  result <- runErrors action
  return $ map snd `leftMap` result

addErr :: (Errors e :> es) => (CallStack, e) -> Eff es ()
addErr e = do
  Errors (errs :| rest) eid aid <- getStaticRep
  putStaticRep $ Errors ((e : errs) :| rest) eid aid

addErrs :: (Errors e :> es) => [(CallStack, e)] -> Eff es ()
addErrs e = do
  Errors (errs :| rest) eid aid <- getStaticRep
  putStaticRep $ Errors ((e ++ errs) :| rest) eid aid

-- | Mark down the error and abort the entire computation.
-- Basically equivalent to `throwError` from the `Error` effect.
throwErr :: (Show e, HasCallStack, Errors e :> es) => e -> Eff es a
throwErr e = do
  Errors {errorId} <- getStaticRep
  addErr (callStack, e)
  withFrozenCallStack throwIO $ ErrorWrapper errorId callStack (show e) (toAny e)

throwErrs :: (Show e, HasCallStack, Errors e :> es) => [e] -> Eff es a
throwErrs errs = do
  forM_ (take (length errs - 1) errs) $ \e ->
    markErr e
  throwErr (last errs)

-- | If any errors have been marked with `markErr` so far, then abort the entire computation
-- (just as if you had called `throwErr`).
abortIfAnyErrors :: (Show e, HasCallStack, Errors e :> es) => Eff es ()
abortIfAnyErrors = do
  Errors {errors, abortId} <- getStaticRep
  case errors of
    [] :| _ -> return ()
    _ -> withFrozenCallStack throwIO $ ErrorWrapper abortId emptyCallStack "" (toAny ())

throwErrWithCallStack :: (Show e, HasCallStack, Errors e :> es) => CallStack -> e -> Eff es a
throwErrWithCallStack cs e = do
  Errors {errorId} <- getStaticRep
  addErr (cs, e)
  withFrozenCallStack throwIO $ ErrorWrapper errorId callStack (show e) (toAny e)

throwErrsWithCallStacks :: (Show e, HasCallStack, Errors e :> es) => [(CallStack, e)] -> Eff es a
throwErrsWithCallStacks errs = do
  Errors {errorId} <- getStaticRep
  addErrs errs
  withFrozenCallStack throwIO $ ErrorWrapper errorId callStack (show (last errs)) (toAny (last errs))

-- | Mark down an error and continue the computation. Once the computation completes, it will
-- fail with this error (and any others that were marked down).
markErr :: (HasCallStack, Errors e :> es) => e -> Eff es ()
markErr e = do
  addErr (callStack, e)

-- | Adapter for running functions that require the `Error` effect when in an `Errors` context.
runErrorAsErrors :: (HasCallStack, Show e, Errors e :> es) => Eff (E.Error e : es) a -> Eff es a
runErrorAsErrors action = do
  result <- E.runError action
  case result of
    Left (cs, e) -> throwErrWithCallStack cs e
    Right x -> return x

-- | Attempt to run the function. If any errors are produced, return them within a `Left`.
tryErr :: (Show e, HasCallStack, Errors e :> es) => Eff es a -> Eff es (Either [(CallStack, e)] a)
tryErr action = do
  staticRep <- getStaticRep

  -- Add a new errors list to the stack, otherwise running the internal effect will mess with the outer effect
  putStaticRep $ staticRep {errors = [] <| staticRep.errors}

  -- Attempt to run the inner effect. If it aborts computation, then save the errors it produced
  result <- catchIf (isMatch . matchError staticRep.errorId staticRep.abortId) (Right <$> action) $ \_ -> do
    Left . NE.head . errors <$> getStaticRep

  staticRep <- getStaticRep

  -- If it didn't abort computation, check to make sure it didn't produce any continuable errors
  result <- case result of
    Left _ -> return result
    Right _ -> do
      (errors :| _) <- errors <$> getStaticRep
      case errors of
        [] -> return result
        errs -> return $ Left errs

  -- Finally, pop the errors list from the stack
  putStaticRep $ staticRep {errors = NE.fromList $ NE.tail staticRep.errors}
  return result

tryErrNoCallStack :: (Show e, HasCallStack, Errors e :> es) => Eff es a -> Eff es (Either [e] a)
tryErrNoCallStack action = do
  result <- tryErr action
  return $ leftMap (map snd) result

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
