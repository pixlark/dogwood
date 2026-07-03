module EmitCSpec (spec) where

import Common
import EmitC.Internal.EmitEffect
import Test.Hspec

testEmitEffect :: Text -> Eff '[Emit] () -> IO ()
testEmitEffect expectedText emitter = do
  let result = runPureEff $ runEmit emitter
  result `shouldBe` expectedText

spec = do
  describe "the EmitC module" do
    describe "the EmitC.Internal.EmitEffect module" do
      it "models the Emit effect correctly" do
        testEmitEffect "foo\nhello!\nbar baz superstar\n" do
          emit "foo\n"
          flush
          emit "bar "
          emit "baz"
          preamble do
            emit "hello!\n"
          emit " superstar\n"
          flush
      it "models multiple layers of the Emit effect correctly" do
        testEmitEffect "baz\nbar\nzap\nfoo\ncrap\n" do
          emit "foo\n"
          preamble do
            emit "bar\n"
            preamble do
              emit "baz\n"
            emit "zap\n"
          emit "crap\n"
