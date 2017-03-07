module HMail.Brick.ViewSwitching where

import HMail.Types
import HMail.Mail
import HMail.Brick.EvH

import Brick.Widgets.Core
import Brick.Widgets.List

import Network.HaskellNet.IMAP.Types


import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Concurrent.Chan

import qualified Data.Vector as V
import qualified Data.Map.Lazy as M


enterMailBoxView :: MailboxName -> EvH ResName ()
enterMailBoxView name =
  whenJustM (use $ mailBoxes . at name) $ \box -> do
    chan <- use cmdChannel
    liftIO $ writeChan chan (FetchMetas name)
    activeView .= MailBoxView name (newList box)
  where
    newList :: MailBox -> List ResName MailMeta
    newList box = list ResMailBoxList (buildVect box) 1

    buildVect :: MailBox -> V.Vector MailMeta
    buildVect box = view mailMeta
      <$> box ^. mails . to (V.fromList . M.elems)


enterBoxesView :: EvH ResName ()
enterBoxesView = do
  chan <- use cmdChannel
  liftIO $ writeChan chan ListMailBoxes
  vec <- use $ mailBoxes . to (V.fromList . M.keys)
  activeView .= MailBoxesView (newList vec)
  where
    newList :: V.Vector MailboxName -> List ResName MailboxName
    newList vec = list ResBoxesList vec 1

enterMailView :: MailMeta -> EvH ResName ()
enterMailView meta =
  error "not implemented yet"

