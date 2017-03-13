{-# language MonadFailDesugaring, GADTs, OverloadedStrings #-}
{-# language LambdaCase, ScopedTypeVariables #-}
{-# language FlexibleContexts, GeneralizedNewtypeDeriving #-}
module HMail.Imap where

import HMail.Util
import HMail.Types
import HMail.Header
import HMail.Mail
import HMail.State

import Brick.BChan

import Network.Socket.Internal (PortNumber)
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.Types
import qualified Network.HaskellNet.IMAP.SSL as Ssl

import qualified Data.Text as T
import Data.Default
import Data.Monoid
import Data.Maybe
import qualified Data.List as L
import qualified Data.Foldable as F

import Control.Lens
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Fail
import Control.Concurrent
import qualified Control.Exception as E


newtype ImapM a = ImapM
  ( ReaderT IMAPConnection IO a )
  deriving (
    Functor
   ,Applicative
   ,Monad
   ,MonadIO
   ,MonadReader IMAPConnection
   ,MonadBase IO
  )

runImapM :: ImapM a
  -> Hostname -> Port -> Username -> Password
  -> IO a
runImapM (ImapM imapAction)
  (Hostname host)
  (Port port)
  (Username user)
  (Password pass) = do
  con <- Ssl.connectIMAPSSLWithSettings host
    (Ssl.defaultSettingsIMAPSSL { Ssl.sslPort = port })
  login con user pass
  runReaderT imapAction con `E.finally` logout con
  
  
liftImapM :: MonadBase m ImapM
  => (IMAPConnection -> m a) -> ImapM a
liftImapM f = ask >>= liftBase . f

liftImapM1 :: MonadBase m ImapM
  => (IMAPConnection -> a -> m b) -> a -> ImapM b
liftImapM1 f x = ask >>= \con -> liftBase $ f con x

liftImapM2 :: MonadBase m ImapM
  => (IMAPConnection -> a -> b -> m c) -> a -> b -> ImapM c
liftImapM2 f x y = ask >>= \con -> liftBase $ f con x y


listMailboxes :: ImapM [(MailboxName,MailBox)]
listMailboxes = genBoxes <$> liftImapM list
  where
    genBoxes = map $ \(attrs,name) -> (name,MailBox mempty attrs)

fetchMetas :: MailboxName -> ImapM [MailMeta]
fetchMetas mbox = do
  liftImapM1 select mbox
  uids <- liftImapM1 search [ALLs]
  forM uids fetchMailMeta

fetchMailContent :: UID -> ImapM MailContent
fetchMailContent uid =
  mkBody <$> liftImapM1 fetch uid


fetchMailMeta :: UID -> ImapM MailMeta
fetchMailMeta uid = do
  hdr <- liftImapM1 fetchHeader uid
  flags <- liftImapM1 fetchFlags uid
  size <- liftImapM1 fetchSize uid
  pure $ MailMeta {
     _metaUid = uid
     ,_metaHeader = parseHeader hdr
     ,_metaFlags = flags
     ,_metaSize = size
    }
    

selectCmd :: Command -> ImapM (Maybe ImapEvent)
selectCmd = \case
  FetchMetas mbox ->
    result (ImapFetchMetas mbox)
      $ fetchMetas mbox
  FetchContent mbox uids ->
    result (ImapFetchContent mbox)
      $ zip uids <$> forM uids fetchMailContent
  ListMailBoxes ->
    result ImapListMailBoxes listMailboxes
  _ -> error "Invalid command. This did not happen."
  where
    result f = fmap (Just . f)


imapThread :: Init
  -> BChan ImapEvent
  -> Chan Command -> IO ()
imapThread init outChan inChan =
  E.try
    ( runImapM action
      ( init ^. imapHostname )
      ( init ^. imapPort     )
      ( init ^. imapUsername )
      ( init ^. imapPassword ) )
  >>= \case
    Left (err :: E.ErrorCall) -> 
      writeBChan outChan (ImapError err)
    Right () -> return ()
  where
    action :: ImapM ()
    action =
      executeCmd outChan ListMailBoxes
      >> loop
    
    loop :: ImapM ()
    loop =
      fetchCmd inChan
      >>= executeCmd outChan
      >> loop

fetchCmd :: Chan Command -> ImapM Command
fetchCmd = liftIO . readChan

executeCmd :: BChan ImapEvent -> Command -> ImapM ()
executeCmd outChan cmd =
  whenJustM (selectCmd cmd)
    (liftIO . writeBChan outChan)
