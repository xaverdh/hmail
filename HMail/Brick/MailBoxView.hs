{-# language LambdaCase, OverloadedStrings, ScopedTypeVariables #-}
module HMail.Brick.MailBoxView (
  handleEvent
, draw
, updateMailBoxView
) where

import HMail.Types
import HMail.ImapMail
import HMail.Header
import HMail.Brick.EventH
import HMail.Brick.Widgets
import HMail.Brick.Util
import HMail.Brick.ViewSwitching
import HMail.Brick.Banner

import Network.HaskellNet.IMAP.Types
import Graphics.Vty.Input.Events

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List

import HMail.Util

import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Base
import Control.Monad.RWS

import Data.Maybe
import Data.Bool
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Map.Lazy as M


handleEvent :: BrickEv e -> EventH MailBoxView ()
handleEvent = \case
  VtyEvent ev -> do
    mblst' <- view boxViewList >>= \case
      Just lst -> do
        lst' <- liftBase $ handleListEvent ev lst
        v <- ask
        tellView $ IsMailBoxView (set boxViewList (Just lst') v)
        pure $ Just lst'
      Nothing -> pure Nothing
    case ev of
      EvKey key mods -> handleKeyEvent mblst' key mods
      _ -> pure ()
  _ -> pure ()


draw :: MailBoxView -> HMailState -> Widget ResName
draw v st = case v ^. boxViewList of
  Just lst ->
    -- padTop (Pad 2)
    banner genericHelp
    <=> renderList renderEntry True lst
  Nothing -> loadingWidget
  where
    renderEntry hasFocus (meta,hdr) =
      withAttr 
        ( attrFocused hasFocus
          <> attrNew (isNew meta) )
      . hBox . map (txt . (<>" ")) . join
      $ catMaybes 
        [ composeId <$> (meta ^? metaUid)
        , composeFlags <$> (meta ^? metaFlags)
        , composeHeader hdr
        , composeSize <$> (meta ^? metaSize) ]
    
    attrFocused = bool mempty $ "focused"
    attrNew = bool mempty $ "new"
    
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
    f s (k,_) = if k < base then (k,s) else (div k base,s)
    base = 10^3
    suffixes = ["K","M","G","T"]


handleKeyEvent :: Maybe (List ResName (MailMeta,Header))
  -> Key -> [Modifier] -> EventH MailBoxView ()
handleKeyEvent mblst key mods = case key of
  KChar 'y' -> enterBoxesView
  KEnter -> whenJust mblst $ \lst ->
    whenJust ( getSelected lst ) $ \(meta,_) -> do
      mbox <- view boxViewName
      boxes <- use mailBoxes
      flip whenJust (enterMailView mbox) $ do
        box <- boxes ^. at mbox
        uid <- meta ^? metaUid
        box ^. mails . at uid -- check that mail exists
        pure uid
  KChar 'r' -> do
    mbox <- view boxViewName
    sendCommand $ FetchMetasAndHeaders mbox
  _ -> pure ()

updateMailBoxView :: EventH MailBoxView ()
updateMailBoxView = do
  name <- view boxViewName
  lst <- newList . vec name <$> get
  v <- ask
  tellView $ IsMailBoxView (set boxViewList (Just lst) v)
  where
    newList xs = list ResMailBoxList xs 1
    vec name st = V.fromList . map extractElem . M.elems
      $ st ^. mailBoxes . ix name . mails
    extractElem mail = (mail ^. immMeta,mail ^. immHeader)

