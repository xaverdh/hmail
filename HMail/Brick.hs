{-# language OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
module HMail.Brick where

import HMail.Util
import HMail.Types
import HMail.State
import HMail.Mail
import HMail.Header
import HMail.Brick.Util


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
   ,appHandleEvent = handleEvent
   ,appStartEvent = startEvent
   ,appAttrMap = const $ attrMap defAttr
    [ ("focused",bg red) ]
  }

startEvent :: HMailState -> EventM n HMailState
startEvent st = pure st


handleEvent :: HMailState
  -> BrickEvent ResName ImapEvent
  -> EventM ResName (Next HMailState)
handleEvent st e = do
  st' <- handleActiveView st e
  case e of
    AppEvent (e::ImapEvent) ->
      handleImapEvent st' e
    VtyEvent (EvKey key mods) -> 
      handleKeyEvent st' key mods
    e -> resizeOrQuit st' e


handleActiveView :: HMailState
  -> BrickEvent ResName ImapEvent
  -> EventM ResName HMailState
handleActiveView st e = 
  case st ^. activeView of
    MailBoxView mbox lst -> 
      MailBoxView.handleEvent mbox lst st e
    MailBoxesView lst ->
      BoxesView.handleEvent lst st e
    MailView uid ->
      MailView.handleEvent uid st e

handleImapEvent :: HMailState
  -> ImapEvent -> EventM ResName (Next HMailState)
handleImapEvent st = \case
  ImapFetchMetas mbox metas ->
    continue $ storeMetas mbox metas st
  ImapFetchContent mbox conts ->
    continue $ storeContents mbox conts st
  ImapListMailBoxes boxData ->
    continue $ storeMailBoxes boxData st
  ImapError err ->
    halt $ logErr err st

handleKeyEvent :: HMailState
  -> Key -> [Modifier]
  -> EventM ResName (Next HMailState)
handleKeyEvent st key mods =
  case key of
    KLeft -> hScrollBy vp (-1) >> next
    KRight -> hScrollBy vp 1 >> next
    KEsc -> quit
    KChar 'r' -> invalidateCache >> next
    KChar 'q' -> quit
{-  KUp -> do
      if haveMod
        then vScrollPage vp Up
        else vScrollBy vp (-1)
      next
    KDown -> do
      if haveMod
        then vScrollPage vp Down
        else vScrollBy vp 1
      next
    KPageUp -> hScrollToBeginning vp >> next
    KPageDown -> hScrollToEnd vp >> next  -}
    _ -> next
  where
    chan = st ^. cmdChannel
    
    haveMod = mods /= []
    
    next = continue st
    quit = halt st
    
    vp = viewportScroll MainViewport


draw :: HMailState -> [Widget ResName]
draw st = pure $ viewport MainViewport Vertical
  $ case st ^. activeView of
  MailBoxesView lst -> BoxesView.draw lst st
  MailBoxView mbox lst -> MailBoxView.draw mbox lst st
  MailView uid -> MailView.draw uid st

load :: Chan Command -> MailboxName -> EventM n ()
load chan mbox =
  liftIO $ writeChan chan (FetchMetas mbox)


