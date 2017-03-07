{-# language LambdaCase, OverloadedStrings #-}
module HMail.Brick.BoxesView where

import HMail.State
import HMail.Types
import HMail.Mail
import HMail.Brick.EvH

import Brick.Types
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.List

import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Control.Lens
import Control.Monad
import Control.Monad.Base
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Concurrent.Chan

import qualified Data.Vector as V
import qualified Data.Map.Lazy as M


handleEvent :: List ResName MailboxName
  -> BrickEvent ResName e -> EvH ResName ()
handleEvent lst = \case
  VtyEvent ev -> do
    lst' <- liftBase $ handleListEvent ev lst
    activeView . boxesViewList .= lst'
    case ev of
      EvKey key mods -> handleKeyEvent lst' key mods
      _ -> pure ()
  _ -> pure ()

draw :: List ResName MailboxName
  -> HMailState -> Widget ResName
draw lst st = renderList renderMBox True lst
  where
    renderMBox :: Bool -> MailboxName -> Widget ResName
    renderMBox focused mbox = 
      ( if focused then withAttr "focused" else id )
      $ str mbox

handleKeyEvent :: List ResName MailboxName
  -> Key -> [Modifier] -> EvH ResName ()
handleKeyEvent lst key mods = 
  case key of
    KEnter -> whenJust mName mEnterMailBoxView
    _ -> pure ()
  where
    mName :: Maybe MailboxName
    mName = fmap
      (\i -> lst ^. listElementsL . ix i)
      (lst ^. listSelectedL)

mEnterMailBoxView :: MailboxName -> EvH ResName ()
mEnterMailBoxView mbox = 
  whenJustM (use $ mailBoxes . at mbox) $ \box -> do
    chan <- use cmdChannel
    liftIO $ writeChan chan (FetchMetas mbox)
    activeView .= MailBoxView mbox (newList box)
  where
    newList :: MailBox -> List ResName MailMeta
    newList box = list ResMailBoxList (buildVect box) 1
    
    buildVect :: MailBox -> V.Vector MailMeta
    buildVect box = view mailMeta
      <$> box ^. mails . to (V.fromList . M.elems)

