{-# language GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module HMail.Brick.EvH where

import Brick.Types
import HMail.Types

import Control.Monad.Trans (lift)
import Control.Monad.Base
import Control.Monad.State
import Control.Monad.State.Class


newtype EvH n a = EvH
  ( StateT HMailState (EventM n) a )
  deriving
    (Functor
    ,Applicative
    ,Monad
    ,MonadIO
    ,MonadState HMailState )

instance MonadBase (EventM n) (EvH n) where
  liftBase m = EvH $ lift m

runEvH :: EvH n a
  -> HMailState
  -> EventM n (a,HMailState)
runEvH (EvH h) = runStateT h

execEvH :: EvH n a
  -> HMailState
  -> EventM n HMailState
execEvH (EvH h) = execStateT h


