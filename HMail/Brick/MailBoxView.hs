module HMail.Brick.MailBoxView where

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Brick.Types
import Brick.Main

import HMail.State
import HMail.Types


handleKeyEvent :: MailboxName -> HMailState
  -> Key -> [Modifier] -> EventM ResName (Next HMailState)
handleKeyEvent mbox st key mods = next
  where
    next = continue st
