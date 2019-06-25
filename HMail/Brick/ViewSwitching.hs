{-# language LambdaCase #-}
module HMail.Brick.ViewSwitching where

import HMail.Types
import HMail.View
import HMail.Header
import HMail.Mail
import HMail.ImapMail
import HMail.Brick.EventH
import HMail.Brick.Util

import Brick.Widgets.Core
import Brick.Widgets.List

import Network.HaskellNet.IMAP.Types

import Control.Lens
import Control.Monad
import Control.Monad.Extra

import qualified Data.Vector as V
import qualified Data.Map.Lazy as M

enterMailBoxView :: MailboxName -> EvH ()
enterMailBoxView name =
  whenJustM (use $ mailBoxes . at name) $ \box -> do
    activeView .= IsMailBoxView ( MailBoxView name (newList box) )
    -- order matters here
    sendCommand $ FetchMetasAndHeaders name
  where
    newList :: MailBox -> List ResName (MailMeta,Header)
    newList box = list ResMailBoxList (buildVect box) 1

    buildVect :: MailBox -> V.Vector (MailMeta,Header)
    buildVect box = extractElem
      <$> box ^. mails . to (V.fromList . M.elems)
    extractElem mail = (mail ^. immMeta,mail ^. immHeader)

enterBoxesView :: EvH ()
enterBoxesView = do
  vec <- use $ mailBoxes . to (V.fromList . M.keys)
  activeView .= IsMailBoxesView ( MailBoxesView (newList vec) )
  -- order matters here
  sendCommand $ ListMailBoxes
  where
    newList :: V.Vector MailboxName -> List ResName MailboxName
    newList vec = list ResBoxesList vec 3


enterMailView :: MailboxName -> UID -> EvH ()
enterMailView mbox uid = do
  activeView .= IsMailView ( MailView mbox uid False )
  -- order matters here
  sendCommand $ FetchContent mbox [uid]

