{-# language LambdaCase #-}
module HMail.Brick.MailView where

import HMail.State
import HMail.Types
import HMail.Brick.EvH

import Brick.Types
import Brick.Main
import Brick.Widgets.List

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events


handleEvent :: UID
  -> BrickEvent ResName e
  -> EvH ResName ()
handleEvent uid e = pure ()

draw :: UID -> HMailState -> Widget ResName
draw uid st = undefined

