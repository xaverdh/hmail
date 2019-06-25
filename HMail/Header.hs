{-# language LambdaCase, TupleSections, OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module HMail.Header where

import Prelude hiding (take)

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.DeepSeq

import Data.Functor
import Data.Bifunctor
import Data.Maybe
import Data.Semigroup
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.ByteString as B
-- import Data.CaseInsensitive (CI)
-- import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8


type HeaderPart = B.ByteString

newtype Header = Header {
    _headerMap :: M.Map HeaderPart T.Text
  }
  deriving (Eq,Ord,Show,NFData)

makeLenses ''Header

asTextMap :: Header -> M.Map T.Text T.Text
asTextMap (Header mp) = M.mapKeys Enc.decodeUtf8 mp

package :: [(B.ByteString,B.ByteString)] -> Header
package = Header . M.fromList
  . map (bimap id Enc.decodeUtf8)

{-

parseHeaderOnly :: B.ByteString -> Header
parseHeaderOnly = package
  . either (const []) id
  . parseOnly headerP


parseHeader :: B.ByteString -> Maybe (Header,B.ByteString)
parseHeader =
  finalise . parse headerP
  where
    finalise = \case
      Fail _ _ _ -> Nothing
      Partial _ -> Nothing
      Done i r -> Just (package r,i)

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
-}

serialiseHeader :: Header -> B.ByteString
serialiseHeader hdr =
  let f key val bs = key <> ":" <> Enc.encodeUtf8 val <> "\n" <> bs
   in M.foldrWithKey f "" (hdr ^. headerMap)

