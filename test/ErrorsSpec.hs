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

    it "preserves error order" do
      let
        e :: Either [String] ()
        e = runPureEff $ runErrorsNoCallStack do
          markErr "first"
          markErr "second"
          markErr "third"
      e `shouldBe` Left ["first", "second", "third"]

    it "handles many accumulated errors" do
      let
        e :: Either [Int] ()
        e = runPureEff $ runErrorsNoCallStack do
          mapM_ markErr [1 .. 100]
      e `shouldBe` Left [1 .. 100]

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

    it "collects errors before throwErr" do
      let
        e :: Either [String] Int
        e = runPureEff $ runErrorsNoCallStack do
          markErr "first"
          markErr "second"
          _ <- throwErr "abort"
          markErr "never reached"
          return 1
      e `shouldBe` Left ["first", "second", "abort"]

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

    it "handles empty computation" do
      let
        e :: Either [String] ()
        e = runPureEff $ runErrorsNoCallStack $ return ()
      e `shouldBe` Right ()

    it "computation result is discarded when errors exist" do
      let
        e :: Either [String] Int
        e = runPureEff $ runErrorsNoCallStack do
          markErr "error"
          return 42
      e `shouldBe` Left ["error"]

    it "multiple throwErr only throws first" do
      ref <- newIORef (0 :: Int)
      let
        e :: Either [String] ()
        e = runPureEff $ runErrorsNoCallStack do
          unsafeEff_ $ modifyIORef ref (+ 1)
          _ <- throwErr "first"
          unsafeEff_ $ modifyIORef ref (+ 1)
          throwErr "second"
      e `shouldBe` Left ["first"]
      readIORef ref `shouldReturn` 1
