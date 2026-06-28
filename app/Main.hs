{-# LANGUAGE QuasiQuotes #-}

module Main where

import Compiler (Compiler (..), execCompilerCallStack, runCompiler, runCompilerCallStack)
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Bifunctor
import qualified Data.Text as T
import Error
import GHC.Exception (prettyCallStack)
import IR (ShowWithSource (..))
import Lexer
import NeatInterpolation
import Parser
import Typechecker (runTypecheckCallStack)

-- main :: IO ()
-- main = case runTypecheckCallStack source of
--   Left (callstack, e) -> do
--     putStrLn $ prettyCallStack callstack
--     putStrLn $ T.unpack $ displayError source e
--   Right a -> print a
--   where
--     source = "{\n    let f: fn(int, int) -> bool = undefined;\n    f(true, 6)\n}\n"

main :: IO ()
main =
  case execCompilerCallStack source of
    Left (cs, e) -> do
      putStrLn $ prettyCallStack cs
      putStrLn $ T.unpack $ displayError source e
    Right Compiler {sealed, program} -> do
      print sealed
      putStrLn $ showWithSource source program
  where
    source =
      [text|
        {
          let n: int = 5;
          let acc: int = 1;
          loop {
            if n == 0 {
              break;
            }
            acc = acc * n;
            n = n - 1;
          }
        }
      |]
