-- |
-- Module      : Common
-- Description : Re-exports commonly used things to augment the prelude.
-- |
module Common
  ( Alternative ((<|>)),
    forM,
    forM_,
    msum,
    unless,
    when,
    isLeft,
    isRight,
    isJust,
    isNothing,
    Text,
    Format,
    Only (..),
    Shown (..),
    format,
    trace,
    traceId,
    traceShow,
    traceShowId,
    Eff,
    runEff,
    runPureEff,
    (:>),
    CallStack,
    Error,
    HasCallStack,
    runError,
    runErrorNoCallStack,
    throwError,
    tryError,
    Reader,
    ask,
    asks,
    local,
    runReader,
    withReader,
    State,
    evalState,
    execState,
    get,
    gets,
    modify,
    put,
    runState,
    state,
    Writer,
    runWriter,
    tell,
    Err (..),
    ErrorKind (..),
    Result,
    Span (..),
    getLineForSpan,
    orThrowSpan,
    throwSpan,
    Log,
    scribe,
    withRegion,
    printf,
    (!?),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM, forM_, msum, unless, when)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text.Format (Format, Only (..), Shown (..), format)
import Debug.Trace (trace, traceId, traceShow, traceShowId)
import Effectful (Eff, runEff, runPureEff, (:>))
import Effectful.Error.Static (CallStack, Error, HasCallStack, runError, runErrorNoCallStack, throwError, tryError)
import Effectful.Reader.Static (Reader, ask, asks, local, runReader, withReader)
import Effectful.State.Static.Local (State, evalState, execState, get, gets, modify, put, runState, state)
import Effectful.Writer.Static.Local (Writer, runWriter, tell)
import Error (Err (..), ErrorKind (..), Result, Span (..), getLineForSpan, orThrowSpan, throwSpan)
import Logging (Log, scribe, withRegion)
import Text.Printf (printf)
import Util ((!?))