{-# language OverloadedStrings #-}
module HMail.Parsing.Mime where

import HMail.Header
import HMail.Types

import qualified Network.Mail.Mime as Mime
import Codec.MIME.Parse
import Codec.MIME.Type
import Codec.MIME.QuotedPrintable


import qualified Data.Text.Encoding as Enc
import qualified Data.Text as T
import qualified Data.ByteString as B

import Text.Parser.Combinators
import Data.Attoparsec.Text
  hiding (choice,sepBy)

parseAddr :: T.Text -> Maybe Mime.Address
parseAddr = either (const Nothing) Just
  . parseOnly (addrP <* endOfInput)

parseAddrs :: T.Text -> Maybe [Mime.Address]
parseAddrs = either (const Nothing) Just
  . parseOnly (addrsP <* endOfInput)

parseMail :: B.ByteString -> Maybe Mail
parseMail bs = do
  (hdr,rst) <- parseHeader bs
  pure Mail {
      _mailContent = mkBody rst
     ,_mailHeader = hdr
     ,_mailMeta = NoMeta
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



addrsP :: Parser [Mime.Address]
addrsP = addrP `sepBy` char ','

addrP :: Parser Mime.Address
addrP = Mime.Address
  <$> optional name
  <*> ( char '<' *> adress <* char '>' )
  where
    name = fmap T.pack . many1 $ satisfy (/= '<')
    adress = fmap T.pack . many $ satisfy (/= '>') 
