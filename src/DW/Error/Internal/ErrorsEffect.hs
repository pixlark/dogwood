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
import Data.Kind
import Data.List (singleton)
import Effectful
import Effectful.Dispatch.Static
import qualified Effectful.Error.Static as E
import Effectful.Exception
import Effectful.Internal.Utils
import GHC.Stack

-- data Errors e :: Effect where
--   ThrowErr :: e -> Errors e m ()
--   MarkErr :: e -> Errors e m ()

-- type instance DispatchOf (Errors e) = Dynamic

-- -- | Throw an error, aborting all computation
-- throwErr :: (Errors e :> es) => e -> Eff es ()
-- throwErr e = send (ThrowErr e)

-- -- | Mark an error for later. The computation will eventually fail, but it
-- -- | continues on in case there are other errors to be accumulated.
-- markErr :: (Errors e :> es) => e -> Eff es ()
-- markErr e = send (MarkErr e)

-- runErrors :: (Show e) => Eff (Errors e : es) a -> Eff es (Either [(CallStack, e)] a)
-- runErrors = reinterpret evaluator $ \_ -> \case
--   ThrowErr e -> E.throwError e
--   MarkErr e -> undefined
--   where
--     evaluator e = do
--       E.runError e

-- data Errors e :: Effect

-- type instance DispatchOf (Errors e) = Static NoSideEffects

-------------------------------------------

-- data Errors e :: Effect

-- type instance DispatchOf (Errors e) = Static NoSideEffects

-- data ErrorsState e = ErrorsState {errors :: [e], continue :: Bool}

-- newtype instance StaticRep (Errors e) = Errors (ErrorsState e)

-- runErrors :: Eff (Errors e : es) a -> Eff es a
-- runErrors = evalStaticRep (Errors $ ErrorsState [] True)

-- throwErr :: (Errors e :> es) => e -> Eff es a
-- throwErr e = do
--   Errors (ErrorsState {errors}) <- getStaticRep
--   putStaticRep $ Errors $ ErrorsState {errors = e : errors, continue = False}
--   return ()

-------------------------------------------

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

-- Taken directly from effectful's implementation

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

-------------------------------------------

-- newtype Errors e a = Errors {runErrors_ :: [e] -> ([e], Maybe a)}

-- instance Functor (Errors e) where
--   fmap :: (a -> b) -> Errors e a -> Errors e b
--   fmap f x = Errors $ \e ->
--     let (errs', x') = runErrors_ x e
--      in (errs', f <$> x')

-- instance Applicative (Errors e) where
--   pure :: a -> Errors e a
--   pure x = Errors (,Just x)

--   (<*>) :: Errors e (a -> b) -> Errors e a -> Errors e b
--   f <*> x = Errors $ \e ->
--     let (e', f') = runErrors_ f e
--         (e'', x') = runErrors_ x e'
--      in (e'', f' <*> x')

-- instance Monad (Errors e) where
--   (>>=) :: Errors e a -> (a -> Errors e b) -> Errors e b
--   x >>= f = Errors $ \e ->
--     let (e', x') = runErrors_ x e
--      in case x' of
--           Nothing -> (e', Nothing)
--           Just x' -> runErrors_ (f x') e'

-- runErrors :: Errors e a -> Either [e] a
-- runErrors e = case (es, x) of
--   ([], Just x) -> Right x
--   ([], Nothing) -> error "unreachable"
--   (es, _) -> Left es
--   where
--     (es, x) = runErrors_ e []

-- throwErr :: e -> Errors e a
-- throwErr e = Errors $ \es -> (e : es, Nothing)

-- markErr :: e -> Errors e ()
-- markErr e = Errors $ \es -> (e : es, Just ())

-----------------------------------

-- f <*> x = Errors $ runErrors f <*> runErrors x

-- instance Functor (Errors e) where
--   fmap :: (a -> b) -> Errors e a -> Errors e b
--   fmap f e = Errors $ f <$> runErrors e

-- instance Applicative (Errors e) where
--   pure :: a -> Errors e a
--   pure = Errors . Right
--   (<*>) :: Errors e (a -> b) -> Errors e a -> Errors e b
--   f <*> x = Errors $ runErrors f <*> runErrors x

-- instance Monad (Errors e) where
--   x >>= f =
--     let x' = runErrors x
--         fx = x' >>= (runErrors . f)
--      in Errors fx

-- throwErr :: e -> Errors e a
-- throwErr = Errors . Left . singleton
