{-# LANGUAGE TypeApplications #-}

module DW.LoopPass (runLoopPass) where

import DW.Common
import DW.Error (markSpan)
import DW.TypedAST
import DW.TypedAST.Visit

newtype LoopPass = LoopPass {inBreakableContext :: Bool}

visitor :: (State LoopPass :> es, Errors Err :> es, Log :> es) => Visitor (Eff es)
visitor = defaultVisitor {onStmt}
  where
    onStmt (TST (Loop _) _) recurse = do
      withRegion "found loop!" do
        saved <- get @LoopPass
        put $ LoopPass True
        _ <- recurse
        put saved
      recurse
    onStmt (TST Break span) recurse = do
      scribe "found break!"
      canBreak <- gets inBreakableContext
      unless canBreak do
        markSpan span BreakOutsideLoop
      recurse
    onStmt _ recurse = recurse

loopPass :: (HasCallStack, Errors Err :> es, Log :> es) => TST Stmt -> Eff es ()
loopPass stmt = evalState (LoopPass False) do
  runStmtVisitor visitor stmt

runLoopPass :: (HasCallStack, Errors Err :> es, Log :> es) => TST Stmt -> Eff es ()
runLoopPass = loopPass
