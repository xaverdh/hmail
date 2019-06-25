{-# language LambdaCase, OverloadedStrings #-}
module HMail.Brick.BoxesView (
  handleEvent
 ,draw
) where

import HMail.Types
import HMail.Brick.Util
import HMail.Brick.ViewSwitching
import HMail.Brick.Banner

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border

import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Control.Lens
import Control.Monad.Base
import Control.Monad.Extra


handleEvent :: MailBoxesView ResName
  -> BrickEvent ResName e -> EvH ()
handleEvent (MailBoxesView lst) = \case
  VtyEvent ev -> do
    lst' <- liftBase $ handleListEvent ev lst
    use activeView >>= \case
      IsMailBoxesView v ->
        activeView .= IsMailBoxesView (set boxesViewList lst' v)
      _ -> pure ()
    case ev of
      EvKey key mods -> handleKeyEvent lst' key mods
      _ -> pure ()
  _ -> pure ()

handleKeyEvent :: List ResName MailboxName
  -> Key -> [Modifier] -> EvH ()
handleKeyEvent lst key _ = case key of
  KEnter -> whenJust (getSelected lst) enterMailBoxView
  KChar 'r' -> sendCommand ListMailBoxes
  _ -> pure ()


draw :: MailBoxesView ResName
  -> HMailState -> Widget ResName
draw (MailBoxesView lst) _ =
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
