module HMail.Brick.Init (
  mkInitialState
) where

import HMail.Types
import Control.Concurrent.Chan
import Data.Monoid
import Data.Map.Lazy as M

mkInitialState :: Chan Command -> HMailState
mkInitialState chan =
  HMailState mempty [] chan MailBoxesView
  -- (MailBoxView "INBOX")
  -- HMailState (M.singleton "inbox" (MailBox mempty []))
  --  [] chan (MailBoxView "inbox")
