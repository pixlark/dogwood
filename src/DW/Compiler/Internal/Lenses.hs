{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DW.Compiler.Internal.Lenses where

import DW.Compiler.Internal.Types
import DW.IR
import DW.Lens

import Data.HashMap.Strict (HashMap)
import GHC.Generics

deriving instance Generic FnDef

makeLenses ''Compiler
makeLenses ''Block
makeLensesWithPrefix "phi" ''Phi
makeLensesWithPrefix "ssa" ''SSA

programIso :: Iso' Program (HashMap FnId FnDef)
programIso = coerced

fnsL :: Lens' FnDef [(BlockId, Block)]
fnsL = gposition @2
