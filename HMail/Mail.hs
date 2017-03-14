{-# language LambdaCase, TemplateHaskell, OverloadedStrings #-}
module HMail.Mail where

import HMail.Types
import HMail.Header
import HMail.Parsing.Mime

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

import qualified Network.Mail.Mime as Mime

-- import Data.Text.ICU.Normalize

mkEmptyMail :: MailMeta -> Header -> Mail
mkEmptyMail meta hdr =
  Mail {
    _mailContent = ContentUnknown
   ,_mailMeta = meta
   ,_mailHeader = hdr
  }


asMimeMail :: Mail -> Maybe Mime.Mail
asMimeMail mail = do
  from <- valueOf "From"
  to <- valuesOf "To"
  cc <- valuesOf "Cc"
  bcc <- valuesOf "Bcc"
  pure (Mime.emptyMail from) {
      Mime.mailTo = to
     ,Mime.mailCc = cc
     ,Mime.mailBcc = bcc
     ,Mime.mailHeaders = rest
     ,Mime.mailParts = undefined
    }
  where
    hdrMap = mail ^. mailHeader . headerMap
    valueOf s = M.lookup s hdrMap >>= parseAddr
    valuesOf s = do
      field <- M.lookup s hdrMap
      parseAddrs field
    special x = x `elem` ["From","To","Cc","Bcc"]
    rest = M.toList $ M.filter (not . special) hdrMap
