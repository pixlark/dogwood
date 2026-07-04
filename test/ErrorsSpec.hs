{-# LANGUAGE TypeApplications #-}

module ErrorsSpec (spec) where

import DW.Common (runPureEff)
import DW.Error.Internal.ErrorsEffect
import Test.Hspec

spec =
  describe "the Errors effect" do
    it "can abort computations" do
      {- HLINT ignore "Use let" -}
      let
        e :: Either [String] Int
        e = runPureEff $ runErrors do
          _ <- return (1 :: Int)
          _ <- return (2 :: Int)
          throwErr "yo what's up"
      e `shouldBe` Left ["yo what's up"]
    it "can log errors and keep going" do
      let
        e :: Either [String] Int
        e = runPureEff $ runErrors do
          _ <- return (1 :: Int)
          markErr "woah that's bad"
          _ <- return (2 :: Int)
          throwErr "now we gotta abort"
      e `shouldBe` Left ["woah that's bad", "now we gotta abort"]
    it "fails if any errors are logged" do
      let
        e :: Either [String] Int
        e = runPureEff $ runErrors do
          x <- return 1
          y <- return 2
          return $ x + y
      e `shouldBe` Right 3
      let
        e :: Either [String] Int
        e = runPureEff $ runErrors do
          x <- return 1
          markErr "here's an error"
          y <- return 2
          return $ x + y
      e `shouldBe` Left ["here's an error"]
