{-# LANGUAGE GADTs #-}

module BSCodeGen where

import qualified Control.Monad.State as S

data LLVMState = LLVMState
  { register :: Int
  }

type GenState = S.State LLVMState

nextRegister :: GenState Int
nextRegister = undefined

