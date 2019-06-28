{-# language OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
module HMail.Brick where

import HMail.Types
import HMail.State
import HMail.Brick.EventH

import qualified HMail.Brick.BoxesView as BoxesView
import qualified HMail.Brick.MailBoxView as MailBoxView
import qualified HMail.Brick.MailView as MailView

import Brick.Types
import Brick.AttrMap
import Brick.Main

import Graphics.Vty (defAttr)
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes

-- import Network.HaskellNet.IMAP.Types

import Control.Monad
import Control.Monad.RWS
import Data.Bifunctor


application :: App (HMailState,View) ImapEvent ResName
application = App {
    appDraw = draw
   ,appChooseCursor = neverShowCursor
   ,appHandleEvent = hmailAppHandleEvent
   ,appStartEvent = startEvent
   ,appAttrMap = attributes
  }
  where
    hmailAppHandleEvent (s,v) e = finaliseEventF (handleEvent e) v s

attributes :: (HMailState,View) -> AttrMap
attributes _ = attrMap defAttr
  $ map (second ($defAttr))
  [ "focused" & style bold
   ,"new" & fgCol green
   ,"header" & style bold . fgCol blue
   ,"banner" & style bold . fgCol yellow . bgCol blue
   ,"body" & id ]
  where
    infixr 1 &
    (&) = (,)
    style = flip withStyle
    fgCol = flip withForeColor
    bgCol = flip withBackColor


startEvent :: (HMailState,View) -> EventM n (HMailState,View)
startEvent st = pure st


handleEvent :: BrickEv ImapEvent -> EventF View
handleEvent e = handleActiveView e *> handleGlobalEvent e

handleGlobalEvent :: BrickEv ImapEvent -> EventF View
handleGlobalEvent = \case
  AppEvent (e::ImapEvent) ->
    handleImapEvent e
  VtyEvent (EvKey key mods) ->
    handleKeyEvent key mods
  e -> resizeOrQuitEventF e


handleActiveView :: BrickEv ImapEvent -> EventH View ()
handleActiveView e = ask >>= \case
  IsMailBoxView v ->
    injectEventH v $ MailBoxView.handleEvent e
  IsMailBoxesView v ->
    injectEventH v $ BoxesView.handleEvent e
  IsMailView v -> 
    injectEventH v $ MailView.handleEvent e

handleImapEvent :: ImapEvent -> EventF View
handleImapEvent = \case
  ImapFetchMetasAndHeaders mbox metasAndHeaders -> do
    storeMetasAndHeaders mbox metasAndHeaders
    ask >>= \case
      IsMailBoxView v ->
        injectEventH v $ MailBoxView.updateMailBoxView
      _ -> pure ()
    continueEventF
  ImapFetchContent mbox uid cont -> do
    storeContent mbox uid cont
    continueEventF
  ImapListMailBoxes boxData -> do
    storeMailBoxes boxData
    ask >>= \case
      IsMailBoxesView v ->
        injectEventH v $ BoxesView.updateBoxesView
      _ -> pure ()
    continueEventF
  ImapError err -> do
    logErr err
    haltEventF


handleKeyEvent :: Key -> [Modifier] -> EventF View
handleKeyEvent key mods =
  case key of
    KEsc -> haltEventF
    KChar 'q' -> haltEventF
    _ -> continueEventF


draw :: (HMailState,View) -> [Widget ResName]
draw (st,someView) = pure $ w -- <=> renderEditor True promptEd
  where
    -- promptEd = editorText ResPrompt (txt . F.fold) (Just 1) "<enter cmd>"
    w = case someView of
      IsMailBoxesView v -> BoxesView.draw v st
      IsMailBoxView v -> MailBoxView.draw v st
      IsMailView v -> MailView.draw v st

