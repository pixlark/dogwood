{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Logging (Log, scribe, runLog, standardLogger, standardLoggerWithIgnoredFunctions, noOpLogger, withRegion) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, putStaticRep, unsafeEff_)
import GHC.Stack (HasCallStack, callStack, getCallStack)
import Text.Printf
import Util (orElse, safeHead)

data Log :: Effect

type instance DispatchOf Log = Static WithSideEffects

newtype Logger = Logger {logMessage :: (HasCallStack) => String -> IO ()}

newtype instance StaticRep Log = Log Logger

dimText :: String -> String
dimText s = "\x1b[1;2m" ++ s ++ "\x1b[0m"

boldText :: String -> String
boldText s = "\x1b[1;1m" ++ s ++ "\x1b[0m"

withRegion :: (HasCallStack, Log :> es) => Text -> Eff es a -> Eff es a
withRegion name f = do
  Log logger <- getStaticRep
  unsafeEff_ $ logMessage logger $ boldText $ Text.unpack name
  let logger' = Logger (\s -> logMessage logger ("  " ++ s))
  putStaticRep (Log logger')
  r <- f
  putStaticRep (Log logger)
  unsafeEff_ $ logMessage logger $ dimText $ printf "/%s" (Text.unpack name)
  return r

scribe :: (HasCallStack, Log :> es) => Text -> Eff es ()
scribe msg = do
  Log logger <- getStaticRep
  unsafeEff_ $ logMessage logger $ Text.unpack msg

runLog :: (IOE :> es) => Logger -> Eff (Log : es) a -> Eff es a
runLog logger = evalStaticRep (Log logger)

standardLogger :: Logger
standardLogger = standardLoggerWithIgnoredFunctions []

standardLoggerWithIgnoredFunctions :: [String] -> Logger
standardLoggerWithIgnoredFunctions ignoredFunctions = Logger {logMessage}
  where
    logMessage :: (HasCallStack) => String -> IO ()
    logMessage msg = do
      let stack = callStack
          ignoreStackNames = ["withRegion", "scribe", "logMessage", "a use of `logMessage'", "$sel:logMessage:Logger"] ++ ignoredFunctions
          fixedStack = filter (not . (`elem` ignoreStackNames) . fst) (getCallStack stack)
          fnName = printf "[%s]" $ fst (safeHead fixedStack `orElse` ("???", undefined))
          padTo = 25
          padBy = max 0 (padTo - length fnName)
          fnName' = fnName ++ replicate padBy ' '
      putStr fnName'
      putStrLn msg

noOpLogger :: Logger
noOpLogger = Logger {logMessage = const (return ())}
