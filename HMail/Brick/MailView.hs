module HMail.Brick.MailView where

import HMail.State
import HMail.Types

import Brick.Types
import Brick.Main

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events


handleKeyEvent :: UID -> HMailState
  -> Key -> [Modifier]
  -> EventM ResName (Next HMailState)
handleKeyEvent uid st key mods = next
  where
    next = continue st
