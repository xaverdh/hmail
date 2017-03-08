{-# language LambdaCase, OverloadedStrings #-}
module HMail.Brick.BoxesView where

import HMail.Types
import HMail.Mail
import HMail.Brick.EvH
import HMail.Brick.Util
import HMail.Brick.ViewSwitching

import Brick.Types
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.List

import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Control.Lens
import Control.Monad
import Control.Monad.Base
import Control.Monad.Extra


handleEvent :: List ResName MailboxName
  -> BrickEvent ResName e -> EvH ResName ()
handleEvent lst = \case
  VtyEvent ev -> do
    lst' <- liftBase $ handleListEvent ev lst
    activeView . boxesViewList .= lst'
    case ev of
      EvKey key mods -> handleKeyEvent lst' key mods
      _ -> pure ()
  _ -> pure ()

handleKeyEvent :: List ResName MailboxName
  -> Key -> [Modifier] -> EvH ResName ()
handleKeyEvent lst key mods = case key of
  KEnter -> whenJust (getSelected lst) enterMailBoxView
  _ -> pure ()


draw :: List ResName MailboxName
  -> HMailState -> Widget ResName
draw lst st = renderList renderMBox True lst
  where
    renderMBox :: Bool -> MailboxName -> Widget ResName
    renderMBox focused mbox = 
      ( if focused then withAttr "focused" else id )
      $ str mbox

