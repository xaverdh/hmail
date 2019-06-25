{-# language LambdaCase, TupleSections, OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module HMail.Header where

import Prelude hiding (take)

import Control.Lens
import Control.DeepSeq

import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.ByteString as B


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

serialiseHeader :: Header -> B.ByteString
serialiseHeader hdr =
  let f key val bs = key <> ":" <> Enc.encodeUtf8 val <> "\n" <> bs
   in M.foldrWithKey f "" (hdr ^. headerMap)

