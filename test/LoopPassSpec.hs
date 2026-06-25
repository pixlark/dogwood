{-# LANGUAGE QuasiQuotes #-}

module LoopPassSpec (spec) where

import Common
import Error
import LoopPass (runLoopPass)
import Test.Hspec
import Prelude hiding (show)

spec = do
  describe "the LoopPass module" do
    it "handles breaks correctly" do
      runLoopPass "loop { break; }" `shouldSatisfy` isRight
      runLoopPass "break;" `shouldSatisfy` isErrorKind BreakOutsideLoop
