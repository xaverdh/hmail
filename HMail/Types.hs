{-# language TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
{-# language GADTs, KindSignatures, TypeFamilies #-}
module HMail.Types where

import HMail.Header
import HMail.TH

import Brick.Types
import Brick.Widgets.List

import qualified Data.Map.Lazy as M
import qualified Data.Text as T
-- import qualified Data.ByteString as B
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

newtype Verbosity = Verbosity Bool

data HMailState =
  HMailState {
   _mailBoxes :: M.Map MailboxName MailBox
   ,_errorLog :: [String]
   ,_cmdChannel :: Chan Command
   ,_verbosity :: Verbosity
  }

data Command =
  FetchContent MailboxName [UID]
  | FetchMetasAndHeaders MailboxName
  | ListMailBoxes


data View =
  IsMailBoxesView MailBoxesView
  | IsMailBoxView MailBoxView
  | IsMailView MailView
  deriving (Show)

data MailBoxesView =
  MailBoxesView {
    _boxesViewList :: List ResName MailboxName
  }
  deriving (Show)

data MailBoxView =
  MailBoxView {
    _boxViewName :: MailboxName
  , _boxViewList :: List ResName (MailMeta,Header)
  }
  deriving (Show)

data MailView =
  MailView {
    _mailViewBoxName :: MailboxName
  , _mailViewUid :: UID
  , _mailViewShowFullHeader :: Bool
  }
  deriving (Eq,Ord,Show)


data MailBox =
  MailBox {
    _mails :: M.Map UID ImapMail
   ,_attrs :: [Attribute]
  } deriving (Eq,Show)


data MailContent =
  ContentIs T.Text
  | ContentUnknown
  deriving (Eq,Ord,Show,Generic,NFData)

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
  } deriving (Eq,Ord,Show,NFData,Generic)

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

makeLenses ''Init
makeLenses ''MailBox
makeLenses ''MailMeta
makeLenses ''ImapMail
makeLenses ''Mail
makeLenses ''HMailState
makeLenses ''MailBoxesView
makeLenses ''MailBoxView
makeLenses ''MailView

makeDType ''Init
myMakeLenses ''DInit
