{-# language FlexibleContexts #-}
module HMail.State where

import HMail.Mail
import HMail.Types
-- import HMail.Brick.EventH

import Network.HaskellNet.IMAP.Types

import Brick.Widgets.List

import qualified Data.Map.Lazy as M
import qualified Data.Vector as V
import qualified Data.Foldable as F
import Control.Lens
import Control.Exception
import Control.Concurrent.Chan
import Control.Monad.State.Class


storeMeta :: MonadState HMailState m
  => MailboxName -> MailMeta -> m ()
storeMeta mbox meta = 
  mailLens .= Just (mkEmptyMail meta)
  where
    uid = meta ^. metaUid
    mailLens = mailBoxes . ix mbox . mails . at uid

storeMetas :: MonadState HMailState m
  => MailboxName -> [MailMeta] -> m ()
storeMetas mbox = mapM_ $ storeMeta mbox

storeContent :: MonadState HMailState m
  => MailboxName -> UID -> MailContent -> m ()
storeContent mbox uid content =
  let mailLens = mailBoxes . ix mbox . mails . ix uid
   in mailLens . mailContent .= content

storeContents :: MonadState HMailState m
  => MailboxName -> [(UID,MailContent)] -> m ()
storeContents mbox = mapM_ (uncurry $ storeContent mbox)

storeMailBoxes :: MonadState HMailState m
  => [(MailboxName,MailBox)] -> m ()
storeMailBoxes boxData = mailBoxes .= M.fromList boxData
  -- M.union (M.fromList boxData)

logErr :: (MonadState HMailState m,Exception e) => e -> m ()
logErr err = errLog %= (show err:)

updateBoxesView :: MonadState HMailState m => m ()
updateBoxesView = do
  lst <- newList . vec <$> get
  activeView . boxesViewList .= lst
  where
    newList v = list ResBoxesList v 1
    vec st = V.fromList . M.keys $ st ^. mailBoxes

updateMailBoxView :: MonadState HMailState m => m ()
updateMailBoxView = do
  lst <- newList . vec <$> get
  activeView . boxViewList .= lst
  where
    name = view (activeView . boxViewName)
    newList v = list ResMailBoxList v 1
    vec st = V.fromList . map (view mailMeta) . M.elems
      $ st ^. mailBoxes . ix (name st) . mails

updateMailView :: MonadState HMailState m => m ()
updateMailView = pure ()


