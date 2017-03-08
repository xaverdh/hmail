module HMail.Brick.ViewSwitching where

import HMail.Types
import HMail.Mail
import HMail.Brick.EvH
import HMail.Brick.Util

import Brick.Widgets.Core
import Brick.Widgets.List

import Network.HaskellNet.IMAP.Types


import Control.Lens
import Control.Monad
import Control.Monad.Extra

import qualified Data.Vector as V
import qualified Data.Map.Lazy as M


enterMailBoxView :: MailboxName -> EvH ResName ()
enterMailBoxView name =
  whenJustM (use $ mailBoxes . at name) $ \box -> do
    activeView .= MailBoxView name (newList box)
    -- order matters here
    sendCommand $ FetchMetas name
  where
    newList :: MailBox -> List ResName MailMeta
    newList box = list ResMailBoxList (buildVect box) 1

    buildVect :: MailBox -> V.Vector MailMeta
    buildVect box = view mailMeta
      <$> box ^. mails . to (V.fromList . M.elems)


enterBoxesView :: EvH ResName ()
enterBoxesView = do
  vec <- use $ mailBoxes . to (V.fromList . M.keys)
  activeView .= MailBoxesView (newList vec)
  -- order matters here
  sendCommand $ ListMailBoxes
  where
    newList :: V.Vector MailboxName -> List ResName MailboxName
    newList vec = list ResBoxesList vec 3

enterMailView :: MailboxName -> Mail -> EvH ResName ()
enterMailView mbox mail = do
  activeView .= MailView mbox uid
  -- order matters here
  sendCommand $ FetchContent mbox [uid]
  where
    uid = mail ^. mailMeta . metaUid

