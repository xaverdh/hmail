{-# language LambdaCase, OverloadedStrings #-}
module HMail.Brick.MailBoxView where

import HMail.Types
import HMail.Header

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Brick.Types
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.List

import HMail.State
import HMail.Types
import HMail.Mail
import HMail.Util

import Control.Lens
import Control.Monad

import Data.Monoid
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Foldable as F

handleEvent :: MailboxName
  -> List ResName MailMeta
  -> HMailState -> BrickEvent ResName e
  -> EventM ResName HMailState
handleEvent mbox lst st = \case
  VtyEvent ev -> do
    lst' <- handleListEvent ev lst
    pure $ st & activeView . boxViewList .~ lst'
  _ -> pure st


draw :: MailboxName
  -> List ResName MailMeta
  -> HMailState -> Widget ResName
draw mbox lst st = 
  renderList renderMMeta True lst
  where
    renderMMeta focused meta =
      hBox . map (txt . (<>" ")) . join
      $ [ composeId (meta ^. metaUid)
         ,composeFlags (meta ^. metaFlags)
         ,composeHeader (meta ^. metaHeader)
         ,composeSize (meta ^. metaSize) ]
    
    composeHeader :: Header -> [T.Text]
    composeHeader hdr =
      let f name = fromMaybe "" (hdr ^. headerMap . at name)
       in map f entries
    
    composeSize :: Int -> [T.Text]
    composeSize s = pure $ "(" <> fmt s <> ")"
    
    composeId :: UID -> [T.Text]
    composeId = pure . showT
    
    composeFlags :: [Flag] -> [T.Text]
    composeFlags = (pure .) . F.foldMap $ \case
      Seen -> ""
      Answered -> "r"
      Flagged -> "f"
      Deleted -> "D"
      Draft -> "d"
      Recent -> ""
      Keyword kw -> "<" <> T.pack kw <> ">"

    entries = ["Date","From","Subject"]
        
    isNew :: MailMeta -> Bool
    isNew meta = flip any (meta ^. metaFlags) $ \case
      Seen -> False
      _ -> True


fmt :: Int -> T.Text
fmt n
  | n < 0 = "bogus size"
  | True = let (n',s) = foldr f (n,"") suffixes in showT n' <> s
  where
    f s (n,_) = if n < base then (n,s) else (div n base,s)
    base = 10^3
    suffixes = ["K","M","G","T"]
