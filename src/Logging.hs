{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Logging (Log, log_, runLog, standardLogger) where

import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)

data Log :: Effect

type instance DispatchOf Log = Static WithSideEffects

newtype Logger = Logger {logMessage :: String -> IO ()}

newtype instance StaticRep Log = Log Logger

log_ :: (Log :> es) => String -> Eff es ()
log_ msg = do
  Log logger <- getStaticRep
  unsafeEff_ $ logMessage logger msg

runLog :: (IOE :> es) => Logger -> Eff (Log : es) a -> Eff es a
runLog logger = evalStaticRep (Log logger)

standardLogger :: Logger
standardLogger = Logger {logMessage}
  where
    logMessage = putStrLn
