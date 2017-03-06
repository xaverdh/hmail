module HMail.Brick.BoxesView where

import HMail.State
import HMail.Types

import Brick.Types
import Brick.Main

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events


handleKeyEvent :: HMailState
  -> Key -> [Modifier]
  -> EventM ResName (Next HMailState)
handleKeyEvent st key mods = next
  where
    next = continue st
