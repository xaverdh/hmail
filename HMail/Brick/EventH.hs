{-# language GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module HMail.Brick.EventH where

import Brick.Main
import Brick.Types

import Control.Monad.Trans (lift)
import Control.Monad.Base
import Control.Monad.State
import Control.Monad.State.Class

newtype EventH s n a = EventH {
    extractEventH :: StateT s (EventM n) a
  }
  deriving
    (Functor
    ,Applicative
    ,Monad
    ,MonadIO
    ,MonadState s )

instance MonadBase (EventM n) (EventH s n) where
  liftBase m = EventH $ lift m

runEventH :: EventH s n a -> s -> EventM n (a,s)
runEventH = runStateT . extractEventH

execEventH :: EventH s n a -> s -> EventM n s
execEventH = execStateT . extractEventH

evalEventH :: EventH s n a -> s -> EventM n a
evalEventH = evalStateT . extractEventH

type EventF s n = EventH s n (Next s)

finaliseEventH :: EventF s n -> s -> EventM n (Next s)
finaliseEventH = evalEventH



continueEventH :: EventF s n
continueEventH = get >>= liftBase . continue

haltEventH :: EventF s n
haltEventH = get >>= liftBase . halt

resizeOrQuitEventH :: BrickEvent n e -> EventF s n
resizeOrQuitEventH ev =
  get >>= liftBase . flip resizeOrQuit ev

suspendAndResumeEventH :: IO s -> EventF s n
suspendAndResumeEventH io =
  liftBase $ suspendAndResume io

