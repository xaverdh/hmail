{-# language LambdaCase #-}
module HMail.View where

import HMail.Types
import HMail.Brick.EventH
import Control.Monad.Writer

tellView :: View -> EvH v ()
tellView = tell . Last . Just

-- fromMailBoxesView :: View -> MailBoxesView
-- fromMailBoxesView = \case
--   IsMailBoxesView v -> v
--   other -> error $ "invalid internal state: wrong view; expected MailBoxes, got: " <> show other
-- 
-- fromMailBoxView :: View -> MailBoxView
-- fromMailBoxView = \case
--   IsMailBoxView v -> v
--   other -> error $ "invalid internal state: wrong view; expected MailBox, got: " <> show other
-- 
-- fromMailView :: View -> MailView
-- fromMailView = \case
--   IsMailView v -> v
--   other -> error $ "invalid internal state: wrong view; expected MailView, got: " <> show other
-- 

