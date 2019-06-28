{-# language GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
module HMail.Brick.EventH (
  EventH(..)
, EventF
, finaliseEventF
, withEventH
, transformEventH
, injectEventH
, continueEventF
, resizeOrQuitEventF
, haltEventF
, suspendAndResumeEventH
) where

import Brick.Main
import Brick.Types
import HMail.Types

import Control.Monad.Trans (lift)
import Control.Monad.Base
import Control.Monad.RWS

import Data.Bifunctor
import Data.Maybe
import Data.Monoid (Last(..))

newtype EventH v a = EventH {
    extractEventH :: RWST v (Last View) HMailState (EventM ResName) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState HMailState
    , MonadReader v
    , MonadWriter (Last View) )

type EventF v = EventH v (Next (Maybe (HMailState,View)))

instance MonadBase (EventM ResName) (EventH v) where
  liftBase m = EventH $ lift m


finaliseEventF :: EventF View -> View -> HMailState
  -> EventM ResName (Next (HMailState,View))
finaliseEventF f v s = do
  (next,s',lv) <- runRWST (extractEventH f) v s
  pure $ fromMaybe (s',defaultView lv) <$> next
  where defaultView lv = fromMaybe v $ getLast lv 


withEventH :: (u -> HMailState -> (v, HMailState))
  -> EventH v a
  -> EventH u a
withEventH g = EventH . withRWST g . extractEventH

transformEventH :: (u -> v) -> EventH v a -> EventH u a
transformEventH f = withEventH $ curry (first f)

injectEventH :: v -> EventH v a -> EventH u a
injectEventH = transformEventH . const

continueEventF :: EventF v
continueEventF = liftBase $ continue Nothing

resizeOrQuitEventF :: BrickEvent ResName e -> EventF v
resizeOrQuitEventF ev = liftBase $ resizeOrQuit Nothing ev

haltEventF :: EventF v
haltEventF = liftBase $ halt Nothing

suspendAndResumeEventH :: IO (Maybe (HMailState,View)) -> EventF v
suspendAndResumeEventH io = do
  liftBase $ suspendAndResume io

