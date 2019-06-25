{-# language LambdaCase #-}
module HMail.Brick.Util where

import HMail.Types

import Brick.Widgets.List

import Control.Lens
import Control.Monad.IO.Class
import Control.Concurrent.Chan

import Data.Vector

getSelected :: List n e -> Maybe e
getSelected lst =
  ((lst ^. listElementsL) !?)
  =<< (lst ^. listSelectedL)

sendCommand :: Command -> EvH ()
sendCommand cmd = do
  chan <- use cmdChannel
  liftIO $ writeChan chan cmd
