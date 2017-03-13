{-# language GADTs, TemplateHaskell, KindSignatures, TypeFamilies #-}
module HMail.Types where

import HMail.Mail
import HMail.Brick.EventH
import HMail.TH

import Brick.Types
import Brick.Widgets.List

import qualified Data.Map.Lazy as M
import Data.Monoid
import Data.Typeable
import Control.Lens
import Control.Exception
import Control.Concurrent.Chan

import Network.Socket.Internal (PortNumber)
import Network.HaskellNet.IMAP.Types

import DTypes.TH


data ImapEvent where
  ImapFetchMetas :: MailboxName -> [MailMeta] -> ImapEvent
  ImapFetchContent :: MailboxName -> [(UID,MailContent)] -> ImapEvent
  ImapListMailBoxes :: [(MailboxName,MailBox)] -> ImapEvent
  ImapError :: Exception e => e -> ImapEvent


newtype Hostname = Hostname String
newtype Port = Port PortNumber
newtype Username = Username String
newtype Password = Password String

data Init = 
  Init {
    _imapHostname :: Hostname
   ,_imapPort :: Port
   ,_imapUsername :: Username
   ,_imapPassword :: Password

   ,_smtpHostname :: Hostname
   ,_smtpPort :: Port
   ,_smtpUsername :: Username
   ,_smtpPassword :: Password
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
  MailBoxesView {
      _boxesViewList :: List n MailboxName
    }
  | MailBoxView { 
      _boxViewName :: MailboxName
     ,_boxViewList :: List n MailMeta
    }
  | MailView {
     _mailViewBoxName :: MailboxName
     ,_mailViewUid :: UID
     ,_mailViewShowFullHeader :: Bool
    }


data MailBox = MailBox {
    _mails :: M.Map UID Mail
   ,_attrs :: [Attribute]
  } deriving (Eq,Show)


data ResName = 
  ResMainViewport
  | ResMailBoxList
  | ResBoxesList
  | ResPrompt
  deriving (Eq,Ord,Show)

type BrickEv = BrickEvent ResName
type EvH = EventH HMailState ResName
type EvF = EventF HMailState ResName

makeLenses ''Init
makeLenses ''MailBox
makeLenses ''HMailState
makeLenses ''View

makeDType ''Init
myMakeLenses ''DInit
