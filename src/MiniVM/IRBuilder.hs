{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- For MonadState s (ModuleBuilderT m) instance
{-# LANGUAGE UndecidableInstances #-}

module MiniVM.IRBuilder where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Fail
import qualified Control.Monad.Fail as Fail
import Control.Monad.Identity
import Control.Monad.List
import Control.Monad.RWS.Lazy as Lazy
import Control.Monad.RWS.Strict as Strict
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Data.Bifunctor
import Data.String
import qualified MiniVM.AST as A

data IRBuilderState = IRBuilderState
  { builderSupply :: !Int,
    builderStatements :: [A.Statement],
    builderModule :: String
  }

newtype IRBuilderT m a = IRBuilderT {unIRBuilderT :: StateT IRBuilderState m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCont,
      MonadError e,
      MonadFix,
      MonadIO,
      MonadReader r,
      MonadTrans,
      MonadWriter w
    )

class Monad m => MonadIRBuilder m where
  liftIRState :: State IRBuilderState a -> m a
  default liftIRState ::
    (MonadTrans t, MonadIRBuilder m1, m ~ t m1) =>
    State IRBuilderState a ->
    m a
  liftIRState = lift . liftIRState

instance Monad m => MonadIRBuilder (IRBuilderT m) where
  liftIRState (StateT s) = IRBuilderT $ StateT $ pure . runIdentity . s

freshReg :: MonadIRBuilder m => m A.Reg
freshReg = liftIRState $ do
  n <- gets builderSupply
  modify $ \s -> s {builderSupply = 1 + n}
  pure $ A.Reg (show n)

buildFunc :: MonadIRBuilder m => String -> m () -> m ()
buildFunc s fn = do
  fn
  liftIRState $ do
    modify $ \s -> s {builderSupply = 0}

buildModule :: MonadIRBuilder m => String -> m () -> m ()
buildModule mname fn = do 
  liftIRState $ do 
    modify $ \s -> s{builderModule = mname}
  fn


