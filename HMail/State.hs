{-# language TemplateHaskell, Strict #-}
module HMail.State where

import HMail.Mail
import HMail.Types

import Network.HaskellNet.IMAP.Types

import Brick.Widgets.List

import qualified Data.Map.Lazy as M
import qualified Data.Vector as V
import qualified Data.Foldable as F
import Control.Lens
import Control.Exception
import Control.Concurrent.Chan


storeMeta :: MailboxName -> MailMeta -> HMailState -> HMailState
storeMeta mbox meta = mailLens .~ Just (mkEmptyMail meta)
  where
    uid = meta ^. metaUid
    mailLens = mailBoxes . ix mbox . mails . at uid

storeMetas :: MailboxName -> [MailMeta] -> HMailState -> HMailState
storeMetas mbox = flip $ foldr (storeMeta mbox)

storeContent :: MailboxName -> UID -> MailContent
  -> HMailState -> HMailState
storeContent mbox uid content =
  let mailLens = mailBoxes . ix mbox . mails . ix uid
   in mailLens . mailContent .~ content

storeContents :: MailboxName -> [(UID,MailContent)]
  -> HMailState -> HMailState
storeContents mbox = flip $ foldr (uncurry $ storeContent mbox)

storeMailBoxes :: [(MailboxName,MailBox)]
  -> HMailState -> HMailState
storeMailBoxes boxData = mailBoxes .~ M.fromList boxData
  -- M.union (M.fromList boxData)

logErr :: Exception e => e -> HMailState -> HMailState
logErr err = errLog %~ (show err:)

updateBoxesView :: HMailState -> HMailState
updateBoxesView st = st &
  activeView . boxesViewList .~ newList
  where
    newList = list ResBoxesList vec 1
    vec = V.fromList . M.keys $ st ^. mailBoxes

updateMailBoxView :: HMailState -> HMailState
updateMailBoxView st = st & 
  activeView . boxViewList .~ newList
  where
    name = st ^. activeView . boxViewName
    newList = list ResMailBoxList vec 1
    vec = V.fromList . map (view mailMeta) . M.elems
      $ st ^. mailBoxes . ix name . mails

