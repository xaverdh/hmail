{-# language LambdaCase #-}
module HMail.Brick.BoxesView where

import HMail.State
import HMail.Types

import Brick.Types
import Brick.Main
import Brick.Widgets.List


import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Control.Lens

handleEvent :: List ResName MailBox
  -> HMailState -> BrickEvent ResName e
  -> EventM ResName HMailState
handleEvent lst st = \case
  VtyEvent ev -> do
    lst' <- handleListEvent ev lst
    pure $ st & activeView . boxesViewList .~ lst'
  _ -> pure st

draw :: List ResName MailBox -> HMailState -> Widget ResName
draw lst st = renderList renderMBox True lst
  where
    renderMBox :: Bool -> MailBox -> Widget ResName
    renderMBox focused box = undefined

