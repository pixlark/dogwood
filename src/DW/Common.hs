-- |
-- Module      : Common
-- Description : Re-exports commonly used things to augment the prelude.
-- |
module DW.Common
  ( Alternative ((<|>)),
    forM,
    forM_,
    msum,
    unless,
    void,
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
    IOE,
    liftIO,
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
    execWriter,
    runWriter,
    tell,
    Err (..),
    ErrorKind (..),
    Errors,
    Result,
    Span (..),
    getLineForSpan,
    orElseMarkSpan,
    orElseMarkSpanM,
    throwSpan,
    orThrowSpan,
    orThrowSpanM,
    orICE,
    orICEM,
    throwErr,
    throwErrs,
    tryErr,
    Log,
    scribe,
    withRegion,
    printf,
    (!?),
    LazyText,
    runErrorAsErrors,
    runErrors,
    runErrorsNoCallStack,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM, forM_, msum, unless, void, when)
import DW.Error
  ( Err (..),
    ErrorKind (..),
    Errors,
    Result,
    Span (..),
    getLineForSpan,
    orElseMarkSpan,
    orElseMarkSpanM,
    orICE,
    orICEM,
    orThrowSpan,
    orThrowSpanM,
    runErrorAsErrors,
    runErrors,
    runErrorsNoCallStack,
    throwErr,
    throwErrs,
    throwSpan,
    tryErr,
  )
import DW.Logging (Log, scribe, withRegion)
import DW.Util ((!?))
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text.Format (Format, Only (..), Shown (..), format)
import Data.Text.Lazy qualified
import Debug.Trace (trace, traceId, traceShow, traceShowId)
import Effectful (Eff, IOE, liftIO, runEff, runPureEff, (:>))
import Effectful.Error.Static (CallStack, Error, HasCallStack, runError, runErrorNoCallStack, throwError, tryError)
import Effectful.Reader.Static (Reader, ask, asks, local, runReader, withReader)
import Effectful.State.Static.Local (State, evalState, execState, get, gets, modify, put, runState, state)
import Effectful.Writer.Static.Local (Writer, execWriter, runWriter, tell)
import Text.Printf (printf)

type LazyText = Data.Text.Lazy.Text
