{-# language OverloadedStrings, LambdaCase #-}
module HMail.Parsing.Mime where

import HMail.Header as H
import HMail.Types
import HMail.Util

import qualified Network.Mail.Mime as Mime
import Data.RFC5322
import Data.MIME
import Data.MIME.TransferEncoding

import qualified Data.Text.Encoding as Enc
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Semigroup
import Data.Either
import qualified Data.Map.Lazy as M
import Data.Bifunctor
import qualified Data.CaseInsensitive as CI

import Control.Lens
import Control.Applicative
import Control.Monad

import Text.Parser.Combinators
import Data.Attoparsec.Text
  hiding (choice,sepBy)
import Data.Attoparsec.ByteString (takeByteString)
import Debug.Trace

parseAddr :: T.Text -> Maybe Mime.Address
parseAddr = either (const Nothing) Just
  . parseOnly (addrP <* endOfInput)

parseAddrs :: T.Text -> Maybe [Mime.Address]
parseAddrs = either (const Nothing) Just
  . parseOnly (addrsP <* endOfInput)


parseHeaderOnly :: B.ByteString -> Maybe H.Header
parseHeaderOnly bs = 
  case Data.MIME.parse (message (const takeByteString)) bs of
    Left err -> Nothing
    Right msg -> Just . package
      $ map (first CI.original) $ view headerList msg

parseMail :: B.ByteString -> Maybe Mail
parseMail bs = do
  msg <- eitherToMaybe $ Data.MIME.parse (message mime) bs
  -- msg :: Message () B.ByteString
  attachmnts <- getAttachmentsMaybe msg
  wire <- getTextPlain msg
  bdy <- eitherToMaybe $ extractTextEnt wire
  pure Mail {
      _mailContent = ContentIs $ view body bdy
     ,_mailHeader = Header . M.fromList $ attachmnts
    }
  where
    eitherToMaybe :: Show a => Either a b -> Maybe b
    eitherToMaybe = either (error . show) Just
    -- eitherToMaybe = either (const Nothing) Just

getAttachmentsMaybe :: MIMEMessage -> Maybe [(B.ByteString, T.Text)]
getAttachmentsMaybe = acc . getAttachments
  where
    acc [] = Just []
    acc ((keyOrErr,maybeVal):ms) = case keyOrErr of
      Left err -> Nothing
      Right key -> case maybeVal of
        Just val -> ((key,val):) <$> acc ms
        Nothing -> Nothing

getAttachments :: MIMEMessage -> [(Either EncodingError B.ByteString, Maybe T.Text)]
getAttachments = toListOf $ attachments . to (liftA2 (,) content name)
  where
    content = view transferDecodedBytes
    name = preview (headers . contentDisposition . filename)

extractTextEnt :: WireEntity -> Either EncodingError TextEntity
extractTextEnt = view transferDecoded >=> view charsetDecoded

getTextPlain :: MIMEMessage -> Maybe WireEntity
getTextPlain = firstOf (entities . filtered f)
  where
  f = matchContentType "text" (Just "plain") . view (headers . contentType)



addrsP :: Parser [Mime.Address]
addrsP = addrP `sepBy` char ','

addrP :: Parser Mime.Address
addrP = Mime.Address
  <$> optional name
  <*> ( char '<' *> adress <* char '>' )
  where
    name = fmap T.pack . many1 $ satisfy (/= '<')
    adress = fmap T.pack . many $ satisfy (/= '>') 
