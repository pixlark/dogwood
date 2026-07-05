{-# LANGUAGE TypeApplications #-}

module ErrorsSpec (spec) where

import DW.Error.Internal.ErrorsEffect

import Data.IORef
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Labeled (labeled, runLabeled)
import GHC.Stack
import Test.Hspec

{- HLINT ignore "Use let" -}
spec =
  describe "the Errors effect" do
    it "can abort computations" do
      let
        e :: Either [String] Int
        e = runPureEff $ runErrorsNoCallStack do
          _ <- return (1 :: Int)
          _ <- return (2 :: Int)
          throwErr "yo what's up"
      e `shouldBe` Left ["yo what's up"]

    it "can log errors and keep going" do
      let
        e :: Either [String] Int
        e = runPureEff $ runErrorsNoCallStack do
          _ <- return (1 :: Int)
          markErr "woah that's bad"
          _ <- return (2 :: Int)
          throwErr "now we gotta abort"
      e `shouldBe` Left ["woah that's bad", "now we gotta abort"]

    it "fails if any errors are logged" do
      let
        e :: Either [String] Int
        e = runPureEff $ runErrorsNoCallStack do
          x <- return 1
          y <- return 2
          return $ x + y
      e `shouldBe` Right 3
      let
        e :: Either [String] Int
        e = runPureEff $ runErrorsNoCallStack do
          x <- return 1
          markErr "here's an error"
          y <- return 2
          return $ x + y
      e `shouldBe` Left ["here's an error"]

    it "does not execute code after throwErr" do
      ref <- newIORef False
      let
        e :: Either [String] ()
        e = runPureEff $ runErrorsNoCallStack do
          _ <- throwErr "abort"
          unsafeEff_ $ writeIORef ref True
      e `shouldBe` Left ["abort"]
      readIORef ref `shouldReturn` False

    it "does execute code after markErr" do
      ref <- newIORef False
      let
        e :: Either [String] ()
        e = runPureEff $ runErrorsNoCallStack do
          markErr "continue"
          unsafeEff_ $ writeIORef ref True
      e `shouldBe` Left ["continue"]
      readIORef ref `shouldReturn` True

    it "handles nested runErrors independently" do
      let
        e :: Either [String] (Either [String] Int)
        e = runPureEff $ runLabeled @"outer" runErrorsNoCallStack do
          labeled @"outer" $ markErr "outer"
          inner <- runLabeled @"inner" runErrorsNoCallStack do
            labeled @"inner" $ markErr "inner"
            return (42 :: Int)
          return inner
      e `shouldBe` Left ["outer"]

    it "nested runErrors don't interfere with each other" do
      let
        e :: Either [String] (Either [Int] Int, Either [Int] Int)
        e = runPureEff $ runErrorsNoCallStack do
          r1 <- runErrorsNoCallStack @Int do
            markErr 1
            return 10
          r2 <- runErrorsNoCallStack @Int do
            return 20
          return (r1, r2)
      e `shouldBe` Right (Left [1], Right 20)

    it "inner throwErr doesn't affect outer" do
      let
        e :: Either [String] (Either [String] Int)
        e = runPureEff $ runLabeled @"outer" runErrorsNoCallStack do
          inner <- runLabeled @"inner" runErrorsNoCallStack do
            labeled @"inner" $ throwErr "inner abort"
          return inner
      e `shouldBe` Right (Left ["inner abort"])

    it "outer throwErr aborts before inner runs" do
      let
        e :: Either [String] (Either [String] Int)
        e = runPureEff $ runErrorsNoCallStack do
          _ <- throwErr "outer abort"
          runErrorsNoCallStack do
            return 42
      e `shouldBe` Left ["outer abort"]

    it "tryErr works" do
      let
        e :: Either [String] (Either [String] Int)
        e = runPureEff $ runErrorsNoCallStack do
          result <- tryErrNoCallStack do
            markErr "error"
            return 5
          return result
      e `shouldBe` Right (Left ["error"])
