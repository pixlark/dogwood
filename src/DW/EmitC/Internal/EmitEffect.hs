{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module DW.EmitC.Internal.EmitEffect (Emit, emit, preamble, flush, abort, runEmit) where

import DW.Common
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Effectful
import Effectful.Dispatch.Dynamic

data Emit :: Effect where
  Emit :: Text -> Emit m ()
  Preamble :: m () -> Emit m ()
  Flush :: Emit m ()
  Abort :: Emit m ()

-- | Write some text into the current `Emit` context.
emit :: (Emit :> es) => Text -> Eff es ()
emit text = send (Emit text)

-- | Open a new `Emit` context. After it's done, everything that it emitted will
-- | be written into the current preamble buffer.
preamble :: (Emit :> es) => Eff es () -> Eff es ()
preamble f = send (Preamble f)

-- | Flush the current preamble buffer, writing it out into the current `Emit` context
-- | (followed by everything that was emitted since the last flush).
flush :: (Emit :> es) => Eff es ()
flush = send Flush

-- | Scrap everything that's been emitted since the last flush
abort :: (Emit :> es) => Eff es ()
abort = send Abort

type instance DispatchOf Emit = Dynamic

data EmitState = EmitState {text :: Builder, emitBuffer :: Builder}

mkEmitState = EmitState {text = mempty, emitBuffer = mempty}

modifyCurrent :: (State (NonEmpty EmitState) :> es) => (EmitState -> EmitState) -> Eff es ()
modifyCurrent f = do
  current <- NE.head <$> get
  let current' = f current
  modify (\(_ :| rest) -> current' :| rest)

runEmit :: Eff (Emit : es) () -> Eff es Text
runEmit = reinterpret evaluator $ \env -> \case
  -- Emitted text gets stored in the emit buffer.
  -- Only after a flush does it get moved to the final text.
  Emit text -> modifyCurrent (\s -> s {emitBuffer = s.emitBuffer <> fromText text})
  Preamble f -> do
    -- Push a new state to the stack
    modify (\e -> mkEmitState :| NE.toList e)
    -- Then, perform the preamble
    localSeqUnlift env $ \unlift -> do
      unlift f
    -- Perform a final flush (in case the user didn't)
    modifyCurrent flusher
    -- Grab the final result from this state
    final <- text . NE.head <$> get
    -- Pop it from the stack
    modify (\(_ :| rest) -> NE.fromList rest)
    -- Then write the final result directly into our text
    modifyCurrent (\s -> s {text = s.text <> final})
  -- Flush everything in the emit buffer into the final text.
  Flush -> modifyCurrent flusher
  -- Abort the current emit buffer, wipe it clean
  Abort -> modifyCurrent (\s -> s {emitBuffer = mempty})
  where
    flusher s = s {text = s.text <> s.emitBuffer, emitBuffer = mempty}
    evaluator e = do
      (s :| _) <- execState (NE.singleton mkEmitState) e
      -- Perform one final flush
      let s' = flusher s
      return $ LazyText.toStrict $ toLazyText s'.text
