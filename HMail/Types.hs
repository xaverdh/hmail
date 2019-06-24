{-# language TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
{-# language GADTs, KindSignatures, TypeFamilies #-}
module HMail.Types where

import HMail.Header
import HMail.Brick.EventH
import HMail.TH

import Brick.Types
import Brick.Widgets.List

import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Semigroup
import Data.Typeable
import Control.Lens
import Control.Exception
import Control.Concurrent.Chan

import Network.Socket.Internal (PortNumber)
import Network.HaskellNet.IMAP.Types

import GHC.Generics (Generic)
import Control.DeepSeq
import DTypes.TH


data ImapEvent where
  ImapFetchMetasAndHeaders :: MailboxName -> [(MailMeta,Header)] -> ImapEvent
  ImapFetchContent :: MailboxName -> UID -> MailContent -> ImapEvent
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
   ,_errorLog :: [String]
   ,_cmdChannel :: Chan Command
   ,_activeView :: View ResName
  }

data Command =
  FetchContent MailboxName [UID]
  | FetchMetasAndHeaders MailboxName
  | ListMailBoxes


data View n = 
  MailBoxesView {
      _boxesViewList :: List n MailboxName
    }
  | MailBoxView { 
      _boxViewName :: MailboxName
     ,_boxViewList :: List n (MailMeta,Header)
    }
  | MailView {
     _mailViewBoxName :: MailboxName
     ,_mailViewUid :: UID
     ,_mailViewShowFullHeader :: Bool
    }


data MailBox = MailBox {
    _mails :: M.Map UID ImapMail
   ,_attrs :: [Attribute]
  } deriving (Eq,Show)


data MailContent =
  ContentIs T.Text
  | ContentUnknown
  deriving (Eq,Show,Generic,NFData)

data MailMeta = 
  MailMeta {
   _metaUid :: UID
   ,_metaFlags :: [Flag]
   ,_metaSize :: Int
  }
  deriving (Eq,Show)

data Mail = Mail {
   _mailContent :: MailContent
  ,_mailHeader :: Header
  } deriving (Eq,Show)

data ImapMail =
  ImapMail {
    _immMeta :: MailMeta
   ,_immMail :: Mail
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
makeLenses ''MailMeta
makeLenses ''ImapMail
makeLenses ''Mail
makeLenses ''HMailState
makeLenses ''View

makeDType ''Init
myMakeLenses ''DInit
