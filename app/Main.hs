{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Text as T
import Error
import GHC.Exception (prettyCallStack)
import Lexer
import Parser
import Typechecker (runTypecheckCallStack)

main :: IO ()
main = case runTypecheckCallStack source of
  Left (callstack, e) -> do
    putStrLn $ prettyCallStack callstack
    putStrLn $ T.unpack $ displayError source e
  Right a -> print a
  where
    source = "{\n    let f: fn(int, int) -> bool = undefined;\n    f(true, 6)\n}\n"
