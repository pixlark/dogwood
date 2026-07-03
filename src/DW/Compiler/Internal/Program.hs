module DW.Compiler.Internal.Program where

import DW.Common
import DW.IR
import DW.Util

class HasProgram s where
  getProgram :: s -> Program
  setProgram :: Program -> s -> s

getBlock ::
  (HasCallStack, State s :> es, HasProgram s, Error Err :> es)
  => BlockId -> Span -> Eff es Block
getBlock id span = do
  (Program blocks) <- gets getProgram
  lookup id blocks `orThrowSpan` (span, InternalCompilerError)

modifyBlock ::
  (HasCallStack, State s :> es, HasProgram s, Error Err :> es)
  => BlockId -> Span -> (Block -> Eff es Block) -> Eff es ()
modifyBlock id span f = do
  (Program blocks) <- gets getProgram
  block <- getBlock id span
  block' <- f block
  let program' = Program $ insertAssoc id block' blocks
  modify (setProgram program')
