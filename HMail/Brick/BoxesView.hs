{-# language LambdaCase, OverloadedStrings #-}
module HMail.Brick.BoxesView (
  handleEvent
, draw
, updateBoxesView
) where

import HMail.Types
import HMail.Brick.EventH
import HMail.View
import HMail.Brick.Util
import HMail.Brick.ViewSwitching
import HMail.Brick.Banner

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border

import qualified Data.Map.Lazy as M
import qualified Data.Vector as V

import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Control.Lens
import Control.Monad.Base
import Control.Monad.Extra
import Control.Monad.RWS


handleEvent :: BrickEv e -> EventH MailBoxesView ()
handleEvent = \case
  VtyEvent ev -> do
    lst <- view boxesViewList
    lst' <- liftBase $ handleListEvent ev lst
    v <- ask
    tellView $ IsMailBoxesView (set boxesViewList lst' v)
    case ev of
      EvKey key mods -> handleKeyEvent lst' key mods
      _ -> pure ()
  _ -> pure ()

handleKeyEvent :: List ResName MailboxName
  -> Key -> [Modifier] -> EventH MailBoxesView ()
handleKeyEvent lst key _ = case key of
  KEnter -> whenJust (getSelected lst) enterMailBoxView
  KChar 'r' -> sendCommand ListMailBoxes
  _ -> pure ()


draw :: MailBoxesView -> HMailState -> Widget ResName
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

updateBoxesView :: EventH MailBoxesView ()
updateBoxesView = do
  lst <- newList . vec <$> get
  v <- ask
  tellView $ IsMailBoxesView (set boxesViewList lst v)
  where
    newList v = list ResBoxesList v 1
    vec st = V.fromList . M.keys $ st ^. mailBoxes


