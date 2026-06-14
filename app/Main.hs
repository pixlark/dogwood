{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Lexer
import Parser

-- main :: IO ()
-- main = do
--   print result
--   print lexer'
--   where
--     lexer = makeLexer "&"
--     run = runState nextToken
--     (result, lexer') = run lexer

main :: IO ()
main = case makeParser lexer of
  Left e -> print e
  Right parser -> print $ run parser
  where
    lexer = makeLexer "bool"
    run = runState $ runExceptT parseBuiltinType
