{-# language LambdaCase, OverloadedStrings #-}
module HMail.Brick.BoxesView (
  handleEvent
, draw
, updateBoxesView
) where

import HMail.Types
import HMail.Brick.EventH
import HMail.Brick.Widgets
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
    mblst' <- view boxesViewList >>= \case
      Just lst -> do
        lst' <- liftBase $ handleListEvent ev lst
        v <- ask
        tellView $ IsMailBoxesView (set boxesViewList (Just lst') v)
        pure $ Just lst'
      Nothing -> pure Nothing
    case ev of
      EvKey key mods -> handleKeyEvent mblst' key mods
      _ -> pure ()
  _ -> pure ()

handleKeyEvent :: Maybe (List ResName MailboxName)
  -> Key -> [Modifier] -> EventH MailBoxesView ()
handleKeyEvent mblst key _ = case key of
  KEnter -> whenJust mblst $ \lst ->
    whenJust (getSelected lst) enterMailBoxView
  KChar 'r' -> sendCommand ListMailBoxes
  _ -> pure ()


draw :: MailBoxesView -> HMailState -> Widget ResName
draw v _ = case v ^. boxesViewList of
  Just lst ->
    banner genericHelp
    <=> renderList (renderMBox $ align lst) True lst
  Nothing -> loadingWidget
  where
    maxLen lst = maximum $ length <$> lst ^. listElementsL
    
    align :: List ResName MailboxName -> String -> String
    align lst s = s <> replicate (maxLen lst - length s) ' '

renderMBox :: (String -> String) -> Bool -> MailboxName -> Widget ResName
renderMBox align focused mbox =
  ( if focused then withAttr "focused" else id )
  . border
  . hCenter
  . str
  . align
  $ mbox


updateBoxesView :: EventH MailBoxesView ()
updateBoxesView = do
  lst <- newList . vec <$> get
  v <- ask
  tellView $ IsMailBoxesView (set boxesViewList (Just lst) v)
  where
    newList v = list ResBoxesList v 1
    vec st = V.fromList . M.keys $ st ^. mailBoxes


