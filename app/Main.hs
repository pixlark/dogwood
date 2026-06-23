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

main :: IO ()
main = case runParseCallStack source parseStmt of
  Left (callstack, e) -> do
    putStrLn $ prettyCallStack callstack
    putStrLn $ T.unpack $ displayError source e
  Right a -> print a
  where
    -- source = "{let x: int = 5; x(15, 16);}"
    source = "{\n    x(15, 16);\n}\n"
