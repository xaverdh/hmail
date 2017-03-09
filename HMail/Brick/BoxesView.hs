{-# language LambdaCase, OverloadedStrings #-}
module HMail.Brick.BoxesView (
  handleEvent
  ,draw
) where

import HMail.Types
import HMail.Mail
import HMail.Brick.EventH
import HMail.Brick.Util
import HMail.Brick.ViewSwitching
import HMail.Brick.Banner

import Brick.Types
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border

import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Control.Lens
import Control.Monad
import Control.Monad.Base
import Control.Monad.Extra
import Data.Monoid

handleEvent :: List ResName MailboxName
  -> BrickEvent ResName e -> EvH ()
handleEvent lst = \case
  VtyEvent ev -> do
    lst' <- liftBase $ handleListEvent ev lst
    activeView . boxesViewList .= lst'
    case ev of
      EvKey key mods -> handleKeyEvent lst' key mods
      _ -> pure ()
  _ -> pure ()

handleKeyEvent :: List ResName MailboxName
  -> Key -> [Modifier] -> EvH ()
handleKeyEvent lst key mods = case key of
  KEnter -> whenJust (getSelected lst) enterMailBoxView
  _ -> pure ()


draw :: List ResName MailboxName
  -> HMailState -> Widget ResName
draw lst st = 
  -- padTop (Pad 5)
  banner genericHelp
  <=> renderList renderMBox True lst
  where
    renderMBox :: Bool -> MailboxName -> Widget ResName
    renderMBox focused mbox = 
      ( if focused then withAttr "focused" else id )
      . border
      . hCenter
      . str
      . align
      $ mbox

    maxLen = maximum $ length <$> lst ^. listElementsL
    
    align :: String -> String
    align s = s <> replicate (maxLen - length s) ' '
