{-# language LambdaCase #-}
module HMail.Brick.ViewSwitching where

import HMail.Types
import HMail.Brick.EventH
import HMail.Header
import HMail.ImapMail
import HMail.Brick.Util

import Brick.Widgets.List

import Network.HaskellNet.IMAP.Types

import Control.Lens
import Control.Monad.Extra

import qualified Data.Vector as V
import qualified Data.Map.Lazy as M

enterMailBoxView :: MailboxName -> EventH v ()
enterMailBoxView name =
  whenJustM (use $ mailBoxes . at name) $ \box -> do
    tellView $ IsMailBoxView ( MailBoxView name (newList box) )
    -- order matters here
    sendCommand $ FetchMetasAndHeaders name
  where
    newList :: MailBox -> List ResName (MailMeta,Header)
    newList box = list ResMailBoxList (buildVect box) 1

    buildVect :: MailBox -> V.Vector (MailMeta,Header)
    buildVect box = extractElem
      <$> box ^. mails . to (V.fromList . M.elems)
    extractElem mail = (mail ^. immMeta,mail ^. immHeader)

enterBoxesView :: EventH v ()
enterBoxesView = do
  tellView $ IsMailBoxesView ( MailBoxesView Nothing )
  -- order matters here
  sendCommand $ ListMailBoxes

enterMailView :: MailboxName -> UID -> EventH v ()
enterMailView mbox uid = do
  tellView $ IsMailView ( MailView mbox uid False )
  -- order matters here
  sendCommand $ FetchContent mbox [uid]

