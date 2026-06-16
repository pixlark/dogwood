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
import Lexer
import Parser

main :: IO ()
main = case runParse source parseExpr of
  Left e -> putStrLn $ T.unpack $ displayError source e
  Right a -> print a
  where
    source = "\n\nfoo(1, 2, 3, return)"
