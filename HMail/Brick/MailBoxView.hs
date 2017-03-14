{-# language LambdaCase, OverloadedStrings, ScopedTypeVariables #-}
module HMail.Brick.MailBoxView where

import HMail.Types
import HMail.Header
import HMail.Brick.EventH
import HMail.Brick.Util
import HMail.Brick.ViewSwitching
import HMail.Brick.Banner

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
import Control.Monad.Extra
import Control.Monad.Base

import Data.Monoid
import Data.Maybe
import Data.Bool
import qualified Data.Text as T
import qualified Data.Foldable as F

handleEvent :: MailboxName
  -> List ResName (MailMeta,Header)
  -> BrickEvent ResName e
  -> EvH ()
handleEvent mbox lst = \case
  VtyEvent ev -> do
    lst' <- liftBase $ handleListEvent ev lst
    activeView . boxViewList .= lst'
    case ev of
      EvKey key mods -> handleKeyEvent lst' key mods
      _ -> pure ()
  _ -> pure ()


draw :: MailboxName
  -> List ResName (MailMeta,Header)
  -> HMailState -> Widget ResName
draw mbox lst st = 
  -- padTop (Pad 2)
  banner genericHelp
  <=> renderList renderEntry True lst
  where
    renderEntry hasFocus (meta,hdr) =
      markFocused hasFocus
      . markNew (isNew meta)
      . hBox . map (txt . (<>" ")) . join
      $ catMaybes 
        [ composeId <$> (meta ^? metaUid)
        , composeFlags <$> (meta ^? metaFlags)
        , composeHeader hdr
        , composeSize <$> (meta ^? metaSize) ]
    
    markFocused = bool id $ withAttr "focused"
    markNew = bool id (withAttr "new")
    
    composeHeader :: Header -> Maybe [T.Text]
    composeHeader hdr =
      let f name = fromMaybe "" (hdr ^. headerMap . at name)
       in Just $ map f entries
    
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

handleKeyEvent :: List ResName (MailMeta,Header)
  -> Key -> [Modifier] -> EvH ()
handleKeyEvent lst key mods = case key of
  KChar 'y' -> enterBoxesView
  KEnter -> whenJust ( getSelected lst ) $ \(meta,_) -> do
    mbox <- use $ activeView . boxViewName
    boxes <- use $ mailBoxes
    flip whenJust (enterMailView mbox) $ do
      box <- boxes ^. at mbox
      uid <- meta ^? metaUid
      box ^. mails . at uid -- check that mail exists
      pure uid
  _ -> pure ()

