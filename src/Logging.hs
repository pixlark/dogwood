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

withRegion :: (HasCallStack, Log :> es) => Text -> Eff es a -> Eff es a
withRegion name f = do
  Log logger <- getStaticRep
  unsafeEff_ $ logMessage logger $ Text.unpack name
  let logger' = Logger (\s -> logMessage logger ("  " ++ s))
  putStaticRep (Log logger')
  r <- f
  putStaticRep (Log logger)
  unsafeEff_ $ logMessage logger $ printf "/{%s}" (Text.unpack name)
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

-- ----------------------------

-- -- | Like Show, but doesn't quote Strings or Chars.
-- class Display a where
--   display :: a -> String

-- instance Display String where display = id

-- instance Display Char where display c = [c]

-- instance Display Int where display = show

-- instance Display Integer where display = show

-- instance Display Double where display = show

-- instance Display Float where display = show

-- instance Display Bool where display = show

-- instance {-# OVERLAPPABLE #-} (Show a) => Display a where display = show

-- class Interpolate r where
--   interpolate' :: [String] -> [String] -> r

-- class InterpolateM r where
--   interpolateM' :: (String -> IO ()) -> [String] -> [String] -> r

-- -- Base case: no more args, produce the final String.
-- instance Interpolate String where
--   interpolate' fragments args = interleave (reverse fragments) (reverse args)

-- instance InterpolateM (IO ()) where
--   interpolateM' action fragments args = action (interleave (reverse fragments) (reverse args))

-- -- Recursive case: consume one more argument, then keep going.
-- instance (Display a, Interpolate r) => Interpolate (a -> r) where
--   interpolate' fragments args a = interpolate' fragments (display a : args)

-- instance (Display a, InterpolateM r) => InterpolateM (a -> r) where
--   interpolateM' action fragments args a = interpolateM' action fragments (display a : args)

-- -- | Interpolate arguments into a template string at "{}" positions.
-- --
-- -- >>> scribe "hello {}, you are {} years old" ("Alice", 30 :: Int)
-- -- "hello Alice, you are 30 years old"
-- --
-- -- >>> scribe "no placeholders" ()
-- -- "no placeholders"
-- interpolate :: (Interpolate r) => String -> r
-- interpolate template = interpolate' (splitOn "{}" template) []

-- interpolateM :: (InterpolateM r) => (String -> IO ()) -> String -> r
-- interpolateM action template = interpolateM' action (splitOn "{}" template) []

-- -- Interleave template fragments with argument strings.
-- interleave :: [String] -> [String] -> String
-- interleave [x] _ = x
-- interleave (x : xs) (a : as) = x ++ a ++ interleave xs as
-- interleave xs _ = concat xs

-- -- Split a string on a delimiter.
-- splitOn :: String -> String -> [String]
-- splitOn _ [] = [""]
-- splitOn sep s
--   | sep == take len s = "" : splitOn sep (drop len s)
--   | otherwise =
--       let (h : t) = splitOn sep (tail s)
--        in (head s : h) : t
--   where
--     len = length sep