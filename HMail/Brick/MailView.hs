{-# language LambdaCase #-}
module HMail.Brick.MailView where

import HMail.State
import HMail.Types

import Brick.Types
import Brick.Main
import Brick.Widgets.List

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events


handleEvent :: UID -> HMailState
  -> BrickEvent ResName e
  -> EventM ResName HMailState
handleEvent uid st e =
  pure $ st

draw :: UID -> HMailState -> Widget ResName
draw uid st = undefined

