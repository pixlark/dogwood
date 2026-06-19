{-# LANGUAGE DataKinds #-}

module Compiler where

import Effectful
import Effectful.Error.Dynamic
import Error

type Compiler a = Eff '[Error Err] a
