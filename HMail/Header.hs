{-# language OverloadedStrings, TemplateHaskell #-}
module HMail.Header where

import Prelude hiding (take)

import Control.Lens
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Bifunctor
import Data.Maybe
import Data.Monoid
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.ByteString as B

-- import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8


type HeaderPart = T.Text

newtype Header = Header {
    _headerMap :: M.Map HeaderPart T.Text
  }
  deriving (Eq,Show)

makeLenses ''Header

parseHeader :: B.ByteString -> Header
parseHeader = Header
  . M.fromList
  . map (bimap Enc.decodeUtf8 Enc.decodeUtf8)
  . either (const []) id
  . parseOnly headerP


headerP :: Parser [(B.ByteString,B.ByteString)]
headerP = many $ lineP where
  lineP = (,) <$> takeWhile1 (/=':') <* take 1 <*> rest
  rest = do
    t <- takeTill (=='\r')
    ts <- string "\r\n" *> ( 
        satisfy isSpace *> rest
        <|> pure ""
      )
    pure (t <> ts)


