{-# language OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
module HMail.Brick where

import HMail.Util
import HMail.Types
import HMail.State
import HMail.Mail
import HMail.Header
import HMail.Brick.Util
import HMail.Brick.EventH

import qualified HMail.Brick.BoxesView as BoxesView
import qualified HMail.Brick.MailBoxView as MailBoxView
import qualified HMail.Brick.MailView as MailView

import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Types
import Brick.AttrMap
import Brick.Main
import Brick.Util

import Graphics.Vty (defAttr)
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes

import Network.HaskellNet.IMAP.Types
import Network.HaskellNet.IMAP

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Monoid
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Map.Lazy as M


application :: App HMailState ImapEvent ResName
application = App {
    appDraw = draw
   ,appChooseCursor = neverShowCursor
   ,appHandleEvent = \s e -> finaliseEventH (handleEvent e) s
   ,appStartEvent = startEvent
   ,appAttrMap = const $ attrMap defAttr
    [ ("focused",defAttr `withStyle` bold) ]
  }

startEvent :: HMailState -> EventM n HMailState
startEvent st = pure st

handleEvent :: BrickEv ImapEvent -> EvF
handleEvent e =
  handleActiveView e >> handleGlobalEvent e

handleGlobalEvent :: BrickEv ImapEvent -> EvF
handleGlobalEvent = \case
  AppEvent (e::ImapEvent) ->
    handleImapEvent e
  VtyEvent (EvKey key mods) -> 
    handleKeyEvent key mods
  e -> resizeOrQuitEventH e

handleActiveView :: BrickEv ImapEvent -> EvH ()
handleActiveView e = use activeView >>= \case
  MailBoxView mbox lst -> 
    MailBoxView.handleEvent mbox lst e
  MailBoxesView lst ->
    BoxesView.handleEvent lst e
  MailView _ uid _ ->
    MailView.handleEvent uid e

handleImapEvent :: ImapEvent -> EvF
handleImapEvent = \case
  ImapFetchMetas mbox metas -> do
    storeMetas mbox metas
    updateMailBoxView
    continueEventH
  ImapFetchContent mbox conts -> do
    storeContents mbox conts
    updateMailView
    continueEventH
  ImapListMailBoxes boxData -> do
    storeMailBoxes boxData
    updateBoxesView
    continueEventH
  ImapError err -> do
    logErr err
    haltEventH

handleKeyEvent :: Key -> [Modifier] -> EvF
handleKeyEvent key mods =
  case key of
    KEsc -> haltEventH
    KChar 'r' -> continueEventH
    KChar 'q' -> haltEventH
    _ -> continueEventH


draw :: HMailState -> [Widget ResName]
draw st = pure $ case st ^. activeView of
  MailBoxesView lst -> BoxesView.draw lst st
  MailBoxView mbox lst -> MailBoxView.draw mbox lst st
  MailView mbox uid fhdr -> MailView.draw mbox uid fhdr st

