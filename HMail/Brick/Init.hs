module HMail.Brick.Init (
  mkInitialState
) where

import HMail.Types
import Control.Concurrent.Chan
import Brick.Widgets.List


mkInitialState :: Chan Command -> HMailState
mkInitialState chan =
  HMailState mempty [] chan (IsMailBoxesView mboxesView)
  where
    mboxesView = MailBoxesView
      (list ResBoxesList mempty 1)
