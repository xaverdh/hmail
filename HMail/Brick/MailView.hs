{-# language LambdaCase, OverloadedStrings #-}
module HMail.Brick.MailView where

import HMail.State
import HMail.Types
import HMail.Mail
import HMail.Header
import HMail.Brick.EventH
import HMail.Brick.ViewSwitching
import HMail.Brick.Banner

import Brick.Types
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Control.Lens
import Control.Monad
import Control.Monad.Base
import Data.Monoid
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

handleEvent :: UID
  -> BrickEvent ResName e
  -> EvH ()
handleEvent uid = \case
  VtyEvent ev -> case ev of
    EvKey key mods -> handleKeyEvent uid key mods
    _ -> pure ()
  _ -> pure ()



handleKeyEvent :: UID -> Key -> [Modifier] -> EvH ()
handleKeyEvent uid key mods = case key of
  KUp -> liftBase $ if haveMod
    then vScrollPage vp Up
    else vScrollBy vp (-1)
  KDown -> liftBase $ if haveMod
    then vScrollPage vp Down
    else vScrollBy vp 1
  KLeft -> liftBase $ if haveMod
    then hScrollBy vp (-10)
    else hScrollBy vp (-1)
  KRight -> liftBase $ if haveMod
    then hScrollBy vp 10
    else hScrollBy vp 1
  KPageUp -> liftBase $ vScrollToBeginning vp
  KPageDown -> liftBase $ vScrollToEnd vp
  KChar 'y' -> do
    mbox <- use $ activeView . mailViewBoxName
    enterMailBoxView mbox
  KChar 'f' -> do
    activeView . mailViewShowFullHeader %= not
  _ -> pure ()
  where
    haveMod = mods /= []
    vp = viewportScroll ResMainViewport


draw :: MailboxName -> UID -> Bool -> HMailState -> Widget ResName
draw mbox uid fullHdr st = 
  (banner mailViewHelp <=>)
  . fromMaybe errorWidget $ do
    box <- st ^. mailBoxes . at mbox
    mail <- box ^. mails . at uid
    Just $ case renderContent (mail ^. mailContent) of 
      Nothing -> loadingWidget
      Just cont -> viewport ResMainViewport Both
        $ renderHeader (mail ^. mailMeta . metaHeader)
        <=> withAttr "body" cont
  where
    renderContent :: MailContent -> Maybe (Widget ResName)
    renderContent = \case
      ContentIs content -> Just $ txt content
      ContentUnknown -> Nothing

    renderHeader :: Header -> Widget ResName
    renderHeader = 
      let f key val wgt = txt (key <> ":" <> val) <=> wgt
       in withAttr "header" 
        . M.foldrWithKey f emptyWidget
        . (if fullHdr then id else M.filterWithKey important)
        . view headerMap
    
    important :: T.Text -> a -> Bool
    important key _ = key `elem`
      [ "Date", "From", "Subject", "To", "User-Agent" ]
    
    loadingWidget = center . border $ txt "LOADING..."
    errorWidget = center . border $ txt "an ERROR ocurred – sorry"
    

mailViewHelp :: [String]        
mailViewHelp = genericHelp ++ [ "f:toggle-full-header" ]

