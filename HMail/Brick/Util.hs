{-# language LambdaCase #-}
module HMail.Brick.Util where

import HMail.Brick.EvH
import HMail.Types

import Brick.Main
import Brick.Types
import Brick.Widgets.List

import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Concurrent.Chan

import Data.Vector

getSelected :: List n e -> Maybe e
getSelected lst =
  ((lst ^. listElementsL) !?)
  =<< (lst ^. listSelectedL)

sendCommand :: Command -> EvH n ()
sendCommand cmd = do
  chan <- use cmdChannel
  liftIO $ writeChan chan cmd
