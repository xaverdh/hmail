{-# language LambdaCase, FlexibleContexts #-}
module HMail.State where

import HMail.Mail
import HMail.ImapMail as ImapMail
import HMail.Types
import HMail.View
import HMail.Header
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


storeMetaAndHeader :: MonadState HMailState m
  => MailboxName -> (MailMeta,Header) -> m ()
storeMetaAndHeader mbox (meta,hdr) =
  let uid = meta ^. metaUid
   in mailLens uid %= Just . f
  where
    f = maybe (ImapMail.mkEmpty meta hdr)
      ( (immMeta .~ meta)
      . (immHeader .~ hdr ) ) 
    mailLens uid = mailBoxes . ix mbox . mails . at uid




storeMetasAndHeaders :: MonadState HMailState m
  => MailboxName -> [(MailMeta,Header)] -> m ()
storeMetasAndHeaders mbox = mapM_ $ storeMetaAndHeader mbox

storeContent :: MonadState HMailState m
  => MailboxName -> UID -> MailContent -> m ()
storeContent mbox uid content =
  let mailLens = mailBoxes . ix mbox . mails . ix uid
   in mailLens . immContent .= content

storeContents :: MonadState HMailState m
  => MailboxName -> [(UID,MailContent)] -> m ()
storeContents mbox = mapM_ (uncurry $ storeContent mbox)

storeMailBoxes :: MonadState HMailState m
  => [(MailboxName,MailBox)] -> m ()
storeMailBoxes boxData = mailBoxes .= M.fromList boxData
  -- M.union (M.fromList boxData)

logErr :: (MonadState HMailState m,Exception e) => e -> m ()
logErr err = errorLog %= (show err:)

logDebug :: MonadState HMailState m => String -> m ()
logDebug s = errorLog %= (s:)

updateBoxesView :: MonadState HMailState m => m ()
updateBoxesView = do
  lst <- newList . vec <$> get
  use activeView >>= \case
    IsMailBoxesView v ->
      activeView .= IsMailBoxesView (set boxesViewList lst v)
    _ -> pure ()
  where
    newList v = list ResBoxesList v 1
    vec st = V.fromList . M.keys $ st ^. mailBoxes

updateMailBoxView :: MonadState HMailState m => m ()
updateMailBoxView = do
  name <- use (activeView . to fromMailBoxView . boxViewName)
  lst <- newList . vec name <$> get
  use activeView >>= \case
    IsMailBoxView v ->
      activeView .= IsMailBoxView (set boxViewList lst v)
    _ -> pure ()
  where
    newList xs = list ResMailBoxList xs 1
    vec name st = V.fromList . map extractElem . M.elems
      $ st ^. mailBoxes . ix name . mails
    extractElem mail = (mail ^. immMeta,mail ^. immHeader)




