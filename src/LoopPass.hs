{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module LoopPass where

import Control.Monad (forM_)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Effectful.State.Static.Local (State, put)
import Error
import TypedAST
import Visit

data LoopPass = LoopPass {inBreakableContext :: Bool}

instance Visitor LoopPass where
  visitStmt (TST (Loop (TST (Body _ stmts) _)) _) = do
    put $ LoopPass {inBreakableContext = True}
  visitStmt _ = return ()

loopPass :: (State LoopPass :> es, Error Err :> es) => [TST Stmt] -> Eff es ()
loopPass stmts = do
  forM_ stmts $ \stmt -> do
    runStmtVisitor stmt
