{-# language LambdaCase, OverloadedStrings #-} module
HMail.Brick.MailView where

import HMail.State
import HMail.Types
import HMail.View
import HMail.Mail
import HMail.ImapMail
import HMail.Header
import HMail.Brick.EventH
import HMail.Brick.Util
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
import Control.Monad.IO.Class
import Data.Semigroup
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

import qualified System.IO as IO

handleEvent :: MailView
  -> BrickEvent ResName e
  -> EvH ()
handleEvent (MailView _ uid _) = \case
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
  KChar ' ' -> liftBase $ vScrollPage vp Down
  KChar 'y' -> do
    mbox <- use $ activeView . to fromMailView . mailViewBoxName
    enterMailBoxView mbox
  KChar 'f' -> do
    use activeView >>= \case
      IsMailView v ->
        activeView .= IsMailView ( (mailViewShowFullHeader %~ not) v )
      _ -> pure ()
  KChar 'r' -> do
    mbox <- use (activeView . to fromMailView . mailViewBoxName)
    uid <- use (activeView . to fromMailView . mailViewUid)
    sendCommand $ FetchContent mbox [uid]
  key -> liftIO . IO.hPutStrLn IO.stderr
    $ "unbound key pressed: " <> show key
  where
    haveMod = mods /= []
    vp = viewportScroll ResMainViewport


draw :: MailView -> HMailState -> Widget ResName
draw (MailView mbox uid fullHdr) st =
  (banner mailViewHelp <=>)
  . fromMaybe errorWidget $ do
    box <- st ^. mailBoxes . at mbox
    mail <- box ^. mails . at uid
    Just $ case renderContent (mail ^. immContent) of 
      Nothing -> loadingWidget
      Just cont -> viewport ResMainViewport Vertical
        $ renderHeader (mail ^. immHeader)
        <=> withAttr "body" cont
  where
    renderContent :: MailContent -> Maybe (Widget ResName)
    renderContent = \case
      ContentIs content -> Just $ txtWrap content
      ContentUnknown -> Nothing

    renderHeader :: Header -> Widget ResName
    renderHeader = 
      let f key val wgt = txtWrap (key <> ":" <> val) <=> wgt
       in withAttr "header"
        . M.foldrWithKey f emptyWidget
        . (if fullHdr then id else M.filterWithKey important)
        . asTextMap
    
    important :: T.Text -> a -> Bool
    important key _ = key `elem`
      [ "Date", "From", "Subject", "To", "User-Agent" ]
    
    loadingWidget = center . border $ txtWrap "LOADING..."
    errorWidget = center . border $ txtWrap "an ERROR ocurred – sorry"
    

mailViewHelp :: [String]        
mailViewHelp = genericHelp ++ [ "f:toggle-full-header" ]

