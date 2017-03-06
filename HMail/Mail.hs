{-# language LambdaCase, TemplateHaskell, OverloadedStrings #-}
module HMail.Mail where

import HMail.Header

import Control.Lens
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Functor
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.ByteString as B


import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types

import Codec.MIME.Parse
import Codec.MIME.Type
import Codec.MIME.QuotedPrintable

-- import Data.Text.ICU.Normalize

data MailContent = 
  ContentIs T.Text
  | ContentUnknown
  deriving (Eq,Show)


data MailMeta = 
  MailMeta {
   _metaUid :: UID
   ,_metaHeader :: Header
   ,_metaFlags :: [Flag]
   ,_metaSize :: Int
  } deriving (Eq,Show)

data Mail = Mail {
   _mailContent :: MailContent
  ,_mailMeta :: MailMeta
  } deriving (Eq,Show)

makeLenses ''MailMeta
makeLenses ''Mail

mkEmptyMail :: MailMeta -> Mail
mkEmptyMail meta =
  Mail {
    _mailContent = ContentUnknown
   ,_mailMeta = meta
  }

mkBody :: B.ByteString -> MailContent
mkBody = ContentIs
  . extract
  . parseMIMEMessage 
  . Enc.decodeUtf8
  where
    extract mime = case mime_val_content mime of
      Single content -> convertNewlines content
      Multi vals -> T.unlines $ map extract vals
    
    convertNewlines = T.replace "\r\n" "\n"

