module DW.Compiler.Internal.Program where

import DW.Common
import DW.IR
import DW.Util

class HasProgram s where
  getProgram :: s -> Program
  setProgram :: Program -> s -> s
