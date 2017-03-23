module HMail.ImapMail where

import qualified HMail.Mail as Mail
import HMail.Types
import HMail.Header

import Control.Lens

mkEmpty :: MailMeta -> Header -> ImapMail
mkEmpty meta hdr =
  ImapMail {
    _immMeta = meta
   ,_immMail = Mail.mkEmpty hdr
  }

immHeader :: Lens' ImapMail Header
immHeader = immMail . mailHeader

immContent :: Lens' ImapMail MailContent
immContent = immMail . mailContent

