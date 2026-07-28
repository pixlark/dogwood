{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module DW.EmitC.Internal.EmitEffect (Emit, emit, preamble, flush, abort, getUnique, runEmit) where

import DW.Common

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Effectful
import Effectful.Dispatch.Dynamic

data Emit :: Effect where
  Emit :: Text -> Emit m ()
  Preamble :: m () -> Emit m ()
  Flush :: Emit m ()
  Abort :: Emit m ()
  GetUnique :: Emit m Text

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

-- | Get a unique string in this emit context
getUnique :: (Emit :> es) => Eff es Text
getUnique = send GetUnique

type instance DispatchOf Emit = Dynamic

data EmitFrame = EmitFrame {text :: Builder, emitBuffer :: Builder}

data EmitState = EmitState {frames :: NonEmpty EmitFrame, uniqCounter :: Int}

mkEmitFrame = EmitFrame {text = mempty, emitBuffer = mempty}

mkEmitState =
  EmitState
    { frames = NE.singleton mkEmitFrame,
      uniqCounter = 0
    }

modifyCurrent :: (State EmitState :> es) => (EmitFrame -> EmitFrame) -> Eff es ()
modifyCurrent f = do
  current <- NE.head <$> gets frames
  let current' = f current
  modifyFrameStack (\(_ :| rest) -> current' :| rest)

modifyFrameStack :: (State EmitState :> es) => (NonEmpty EmitFrame -> NonEmpty EmitFrame) -> Eff es ()
modifyFrameStack f =
  modify (\s -> s {frames = f s.frames})

runEmit :: Eff (Emit : es) () -> Eff es Text
runEmit = reinterpret evaluator $ \env -> \case
  -- Emitted text gets stored in the emit buffer.
  -- Only after a flush does it get moved to the final text.
  Emit text -> modifyCurrent (\s -> s {emitBuffer = s.emitBuffer <> fromText text})
  Preamble f -> do
    -- Push a new state to the stack
    modifyFrameStack (\e -> mkEmitFrame :| NE.toList e)
    -- Then, perform the preamble
    localSeqUnlift env $ \unlift -> do
      unlift f
    -- Perform a final flush (in case the user didn't)
    modifyCurrent flusher
    -- Grab the final result from this state
    final <- text . NE.head <$> gets frames
    -- Pop it from the stack
    modifyFrameStack (\(_ :| rest) -> NE.fromList rest)
    -- Then write the final result directly into our text
    modifyCurrent (\s -> s {text = s.text <> final})
  -- Flush everything in the emit buffer into the final text.
  Flush -> modifyCurrent flusher
  -- Abort the current emit buffer, wipe it clean
  Abort -> modifyCurrent (\s -> s {emitBuffer = mempty})
  -- Get a unique string
  GetUnique -> do
    uniq <- gets uniqCounter
    modify (\s -> s {uniqCounter = uniq + 1})
    return $ Text.pack $ printf "_%d" uniq
  where
    flusher s = s {text = s.text <> s.emitBuffer, emitBuffer = mempty}
    evaluator e = do
      s <- execState mkEmitState e
      let f = NE.head s.frames
      -- Perform one final flush
      let f' = flusher f
      return $ LazyText.toStrict $ toLazyText f'.text
