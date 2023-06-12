{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
import qualified Data.DList as DL
import Data.String
import qualified MiniVM.AST as A

data IRBuilderState = IRBuilderState
  { builderSupply :: !Int,
    builderStatements :: DL.DList A.Statement,
    builderModule :: !String,
    builderFunction :: !(Maybe String),
    builderLoopLabelSupply :: !Int, 
    builderCondLabelSupply :: !Int
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
    modify $ \s -> s {builderSupply = 0, builderLoopLabelSupply = 0}

buildModule :: MonadIRBuilder m => String -> m () -> m ()
buildModule mname fn = do
  liftIRState $ do
    modify $ \s -> s {builderModule = mname}
  fn

buildStatement :: MonadIRBuilder m => A.Statement -> m ()
buildStatement stmt = liftIRState $ do
  dlist <- gets builderStatements
  let dlist' = DL.insert stmt dlist
  modify $ \s -> s {builderStatements = dlist'}

buildLabel :: MonadIRBuilder m => String -> m A.LabelName
buildLabel lname = do
  let label = A.LabelName lname
  let stmt = A.Label label
  buildStatement stmt
  pure label

buildLoopHeader :: MonadIRBuilder m => m A.LabelName
buildLoopHeader = do 
    label <- liftIRState $ do
      mname <- gets builderModule
      fname <- gets builderFunction
      supply <- gets builderLoopLabelSupply
      case fname of
        Just fname -> do
          modify $ \s -> s {builderLoopLabelSupply = supply + 1}
          pure (mname ++ "." ++ fname ++ "." ++ show supply)
        Nothing -> error "buildLoopHeader must be called inside buildFunc" 
    buildLabel label


buildLoopHeaderEnd :: MonadIRBuilder m =>  A.LabelName -> m A.LabelName 
buildLoopHeaderEnd (A.LabelName lname) = do 
  let endName = lname ++ ".end"
  buildLabel endName


