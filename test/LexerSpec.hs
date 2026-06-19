{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LexerSpec (spec) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Loops (unfoldM)
import Control.Monad.State
import Data.Text (Text)
import Error
import Lexer
import Test.Hspec

runLex :: Text -> Result [Token]
runLex source = run lexer
  where
    lexer = makeLexer source
    maybeNextToken = do
      tok <- nextToken
      return $ if tok.kind == Eof then Nothing else Just tok
    run = evalState $ runExceptT $ unfoldM maybeNextToken

spec =
  describe "the Lexer module" $ do
    it "tracks source spans correctly" $ do
      runLex "asdf, void "
        `shouldBe` Right
          [ Token (Symbol "asdf") (Span 0 4),
            Token (Glyph ",") (Span 4 1),
            Token (Keyword "void") (Span 6 4)
          ]
