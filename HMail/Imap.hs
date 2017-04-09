{-# language MonadFailDesugaring, GADTs, OverloadedStrings #-}
{-# language LambdaCase, ScopedTypeVariables #-}
{-# language FlexibleContexts, GeneralizedNewtypeDeriving #-}
module HMail.Imap where

import HMail.Util
import HMail.Types
import HMail.Header
import HMail.Mail
import HMail.State
import HMail.Parsing.Mime (mkBody)

import Brick.BChan

import Network.Socket.Internal (PortNumber)
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.Types
import qualified Network.HaskellNet.IMAP.SSL as Ssl

import qualified Data.Text as T
import qualified Data.ByteString as B
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
import Control.DeepSeq

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

fetchAll :: MailboxName -> (UID -> ImapM a) -> ImapM [a]
fetchAll mbox action = do
  liftImapM1 select mbox
  uids <- liftImapM1 search [ALLs]
  forM uids action

fetchMailContent :: UID -> ImapM B.ByteString
fetchMailContent uid =
   liftImapM1 fetch uid


fetchMailMeta :: UID -> ImapM MailMeta
fetchMailMeta uid = do
  flags <- liftImapM1 fetchFlags uid
  size <- liftImapM1 fetchSize uid
  pure $ MailMeta {
     _metaUid = uid
     ,_metaFlags = flags
     ,_metaSize = size
    }

fetchMailHeader :: UID -> ImapM Header
fetchMailHeader uid = do
  parseHeaderOnly <$> liftImapM1 fetchHeader uid


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

writeRes :: BChan ImapEvent -> ImapEvent -> ImapM ()
writeRes outChan = liftIO . writeBChan outChan

executeCmd :: BChan ImapEvent -> Command -> ImapM ()
executeCmd outChan = \case  
  FetchMetasAndHeaders mbox -> do
    dat <- fetchAll mbox
      ( \uid -> (,)
        <$> fetchMailMeta uid
        <*> fetchMailHeader uid )
    result (ImapFetchMetasAndHeaders mbox) dat
  FetchContent mbox uids -> do
    forM uids $ \uid -> do
      cont <- fetchMailContent uid
      liftIO . forkIO $ parsingThread mbox uid cont
    pure ()
  ListMailBoxes ->
    listMailboxes >>= result ImapListMailBoxes
  _ -> error "Invalid command. This did not happen."
  where
    result :: (a -> ImapEvent) -> a -> ImapM () 
    result f = writeRes outChan . f

    parsingThread mbox uid cont = do
      cont' <- E.evaluate $ force $ mkBody cont
      writeBChan outChan $
        ImapFetchContent mbox uid cont'


