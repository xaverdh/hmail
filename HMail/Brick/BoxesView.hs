{-# language LambdaCase, OverloadedStrings #-}
module HMail.Brick.BoxesView where

import HMail.State
import HMail.Types

import Brick.Types
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.List


import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Control.Lens

handleEvent :: List ResName MailboxName
  -> HMailState -> BrickEvent ResName e
  -> EventM ResName HMailState
handleEvent lst st = \case
  VtyEvent ev -> do
    lst' <- handleListEvent ev lst
    pure $ st & activeView . boxesViewList .~ lst'
  _ -> pure st

draw :: List ResName MailboxName
  -> HMailState -> Widget ResName
draw lst st = renderList renderMBox True lst
  where
    renderMBox :: Bool -> MailboxName -> Widget ResName
    renderMBox focused mbox = 
      ( if focused then withAttr "focused" else id )
      $ str mbox


