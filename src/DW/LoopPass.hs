{-# LANGUAGE TypeApplications #-}

module DW.LoopPass (runLoopPass) where

import DW.Common
import DW.TypedAST
import DW.Visit

newtype LoopPass = LoopPass {inBreakableContext :: Bool}

visitor :: (State LoopPass :> es, Error Err :> es) => Visitor (Eff es)
visitor = defaultVisitor {onStmt}
  where
    onStmt (TST (Loop _) _) recurse = do
      saved <- get @LoopPass
      put $ LoopPass True
      _ <- recurse
      put saved
    onStmt (TST Break span) recurse = do
      canBreak <- gets inBreakableContext
      unless canBreak $ throwSpan span BreakOutsideLoop
      recurse
    onStmt _ _ = return ()

loopPass :: (Error Err :> es) => TST Stmt -> Eff es ()
loopPass stmt = do
  let result = runPureEff $ runErrorNoCallStack $ evalState (LoopPass False) $ runStmtVisitor visitor stmt
  case result of
    Left e -> throwError e
    Right () -> return ()

runLoopPass :: (HasCallStack, Error Err :> es) => TST Stmt -> Eff es ()
runLoopPass = loopPass