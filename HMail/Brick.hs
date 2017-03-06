{-# language OverloadedStrings, LambdaCase #-}
module HMail.Brick where

import HMail.Util
import HMail.Types
import HMail.State
import HMail.Mail
import HMail.Header

import qualified HMail.Brick.BoxesView as BoxesView
import qualified HMail.Brick.MailBoxView as MailBoxView
import qualified HMail.Brick.MailView as MailView

import Brick.Widgets.Core
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
startEvent st = do
  -- load (st ^. cmdChannel) "INBOX"
  pure st


handleEvent :: HMailState
  -> BrickEvent ResName ImapEvent
  -> EventM ResName (Next HMailState)
handleEvent st = \case
  AppEvent e -> handleImapEvent st e
  VtyEvent (EvKey key mods) -> 
    handleKeyEvent st key mods
  e -> resizeOrQuit st e


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
    KUp -> do
      if haveMod
        then vScrollPage vp Up
        else vScrollBy vp (-1)
      next
    KDown -> do
      if haveMod
        then vScrollPage vp Down
        else vScrollBy vp 1
      next
    KLeft -> hScrollBy vp (-1) >> next
    KRight -> hScrollBy vp 1 >> next
    KPageUp -> hScrollToBeginning vp >> next
    KPageDown -> hScrollToEnd vp >> next
    KEsc -> quit
    KChar 'r' -> invalidateCache >> next
    KChar 'q' -> quit
    _ -> case st ^. activeView of
      MailBoxView mbox -> 
        MailBoxView.handleKeyEvent mbox st key mods
      MailBoxesView ->
        BoxesView.handleKeyEvent st key mods
      MailView uid ->
        MailView.handleKeyEvent uid st key mods
  where
    chan = st ^. cmdChannel
    
    haveMod = mods /= []
    
    next = continue st
    quit = halt st
    
    vp = viewportScroll MainViewport


draw :: HMailState -> [Widget ResName]
draw st = case st ^. activeView of
  MailBoxView mbox -> pure $ mailBoxView mbox st
  MailBoxesView -> pure $ mailBoxesView st
  MailView uid -> pure $ mailView uid st


mailView :: UID -> HMailState -> Widget ResName
mailView uid = undefined


mailBoxesView :: HMailState -> Widget ResName
mailBoxesView st = viewport MainViewport Both
  . padLeftRight 5
  $ txt "Mailboxes:" <=> lst
  where
    sep = replicate 2 $ txt " "
    lst = vBox $ do
      (name,box) <- M.toList (st ^. mailBoxes)
      sep ++ [str name]

{-
mailBoxesView :: HMailState -> Widget ResName
mailBoxesView st = viewport MainViewport Both
  . padLeftRight 5
  $ txt "Mailboxes:" <=> lst
  where
    sep = replicate 2 $ txt " "
    lst = vBox $ do
      (name,box) <- M.toList (st ^. mailBoxes)
      sep ++ [str name]
-}

mailBoxView :: MailboxName -> HMailState -> Widget ResName
mailBoxView mbox st = viewport MainViewport Both
  $ txt "Mails:" <=> lst
  where
    lst = vBox $ do
      meta <- view mailMeta
        <$> M.elems (st ^. mailBoxes . ix mbox . mails)
      pure $ hBox (composeEntry meta)
    
    entries = ["Date","From","Subject"]
    
    composeEntry :: MailMeta -> [Widget ResName]
    composeEntry meta = map (txt . (<>" ")) $ join
      [ composeId (meta ^. metaUid)
       ,composeFlags (meta ^. metaFlags)
       ,composeHeader (meta ^. metaHeader)
       ,composeSize (meta ^. metaSize) ]
    
    composeHeader :: Header -> [T.Text]
    composeHeader hdr =
      let f name = fromMaybe "" (hdr ^. headerMap . at name)
       in map f entries
    
    composeSize :: Int -> [T.Text]
    composeSize s = pure $ "(" <> fmt s <> ")"
    
    composeId :: UID -> [T.Text]
    composeId = pure . showT
    
    composeFlags :: [Flag] -> [T.Text]
    composeFlags = (pure .) . F.foldMap $ \case
      Seen -> ""
      Answered -> "r"
      Flagged -> "f"
      Deleted -> "D"
      Draft -> "d"
      Recent -> ""
      Keyword kw -> "<" <> T.pack kw <> ">"

    isNew :: MailMeta -> Bool
    isNew meta = flip any (meta ^. metaFlags) $ \case
      Seen -> False
      _ -> True


fmt :: Int -> T.Text
fmt n 
  | n < 0 = "bogus size"
  | True = let (n',s) = foldr f (n,"") suffixes in showT n' <> s
  where
    f s (n,_) = if n < base then (n,s) else (div n base,s)
    base = 10^3
    suffixes = ["K","M","G","T"]


load :: Chan Command -> MailboxName -> EventM n ()
load chan mbox =
  liftIO $ writeChan chan (FetchMetas mbox)

