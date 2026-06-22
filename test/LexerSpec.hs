{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LexerSpec (spec) where

import Control.Monad
import Control.Monad.Loops (unfoldM)
import Data.Bifunctor (first)
import Data.Either
import Data.Text (Text)
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
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
    run l = runPureEff $ evalState l $ runErrorNoCallStack $ unfoldM maybeNextToken

-- run l = do
--   startingState <- get
--   result <- tryError $ unfoldM maybeNextToken
--   return undefined

spec =
  describe "the Lexer module" $ do
    it "tracks source spans correctly" $ do
      runLex "asdf, void "
        `shouldBe` Right
          [ Token (Symbol "asdf") (Span 0 4),
            Token (Glyph ",") (Span 4 1),
            Token (Keyword "void") (Span 6 4)
          ]
