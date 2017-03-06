{-# language GADTs, LambdaCase, TemplateHaskell #-}
module HMail.Types where

import HMail.Mail

import Data.Monoid
import Data.Typeable
import Control.Lens
import Control.Exception
import Control.Concurrent.Chan

import Network.Socket.Internal (PortNumber)
import Network.HaskellNet.IMAP.Types
import qualified Data.Map.Lazy as M

import Brick.Widgets.List


data ImapEvent where
  ImapFetchMetas :: MailboxName -> [MailMeta] -> ImapEvent
  ImapFetchContent :: MailboxName -> [(UID,MailContent)] -> ImapEvent
  ImapListMailBoxes :: [(MailboxName,MailBox)] -> ImapEvent
  ImapError :: Exception e => e -> ImapEvent

data ImapInit = 
  ImapInit {
    _imapHostname :: String
   ,_imapPort :: PortNumber
   ,_imapUsername :: String
   ,_imapPassword :: String
  }

data HMailState = 
  HMailState {
   _mailBoxes :: M.Map MailboxName MailBox
   ,_errLog :: [String]
   ,_cmdChannel :: Chan Command
   ,_activeView :: View ResName
  }

data Command =
  FetchContent MailboxName [UID]
  | FetchMetas MailboxName
  | ListMailBoxes

data View n = 
  MailBoxesView (List n MailBox)
  | MailBoxView MailboxName (List n MailMeta)
  | MailView UID

data MailBox = MailBox {
    _mails :: M.Map UID Mail
   ,_attrs :: [Attribute]
  } deriving (Eq,Show)


data ResName = MainViewport
  deriving (Eq,Ord,Show)

makeLenses ''ImapInit
makeLenses ''MailBox
makeLenses ''HMailState


