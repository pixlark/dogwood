{-# LANGUAGE TypeApplications #-}

module DW.LoopPass (runLoopPass) where

import DW.Common
import DW.Error (markSpan)
import DW.TypedAST
import DW.TypedAST.Visit

newtype LoopPass = LoopPass {inBreakableContext :: Bool}

visitor :: (State LoopPass :> es, Errors Err :> es) => Visitor (Eff es)
visitor = defaultVisitor {onStmt}
  where
    onStmt (TST (Loop _) _) recurse = do
      saved <- get @LoopPass
      put $ LoopPass True
      _ <- recurse
      put saved
    onStmt (TST Break span) recurse = do
      canBreak <- gets inBreakableContext
      unless canBreak do
        markSpan span BreakOutsideLoop
      recurse
    onStmt _ _ = return ()

loopPass :: (Errors Err :> es) => TST Stmt -> Eff es ()
loopPass stmt = do
  let result = runPureEff $ runErrorsNoCallStack $ evalState (LoopPass False) $ runStmtVisitor visitor stmt
  case result of
    Left e -> throwErrs e
    Right () -> return ()

runLoopPass :: (HasCallStack, Errors Err :> es) => TST Stmt -> Eff es ()
runLoopPass = loopPass
