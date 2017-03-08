{-# language LambdaCase, OverloadedStrings #-}
module HMail.Brick.MailView where

import HMail.State
import HMail.Types
import HMail.Mail
import HMail.Header
import HMail.Brick.EvH
import HMail.Brick.ViewSwitching

import Brick.Types
import Brick.Main
import Brick.Widgets.Core

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Control.Lens
import Control.Monad
import Control.Monad.Base
import Data.Monoid
import Data.Maybe
import qualified Data.Map.Lazy as M

handleEvent :: UID
  -> BrickEvent ResName e
  -> EvH ResName ()
handleEvent uid = \case
  VtyEvent ev -> case ev of
    EvKey key mods -> handleKeyEvent uid key mods
    _ -> pure ()
  _ -> pure ()


handleKeyEvent :: UID -> Key -> [Modifier] -> EvH ResName ()
handleKeyEvent uid key mods = case key of
  KUp -> liftBase $ if haveMod
    then vScrollPage vp Up
    else vScrollBy vp (-1)
  KDown -> liftBase $ if haveMod
    then vScrollPage vp Down
    else vScrollBy vp 1
  KLeft -> liftBase $ hScrollBy vp (-1)
  KRight -> liftBase $ hScrollBy vp 1
  KPageUp -> liftBase $ vScrollToBeginning vp
  KPageDown -> liftBase $ vScrollToEnd vp
  KChar 'y' -> do
    mbox <- use $ activeView . mailViewBoxName
    enterMailBoxView mbox
  _ -> pure ()
  where
    haveMod = mods /= []
    vp = viewportScroll ResMainViewport

draw :: MailboxName -> UID -> HMailState -> Widget ResName
draw mbox uid st = 
  viewport ResMainViewport Both
  . fromMaybe errorWidget $ do
    box <- st ^. mailBoxes . at mbox
    mail <- box ^. mails . at uid
    Just $ renderHeader (mail ^. mailMeta . metaHeader)
      <=> renderContent (mail ^. mailContent)
  where
    renderContent :: MailContent -> Widget ResName
    renderContent = \case
      ContentIs content -> txt content
      ContentUnknown -> txt ""

    renderHeader :: Header -> Widget ResName
    renderHeader =
      let f key val wgt = txt (key <> ":" <> val) <=> wgt
       in M.foldrWithKey f emptyWidget . view headerMap
    
    errorWidget = txt "An error ocurred – Sorry."
    
        
