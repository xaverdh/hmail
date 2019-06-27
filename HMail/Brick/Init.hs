module HMail.Brick.Init (
  mkInitialState
) where

import HMail.Types
import Control.Concurrent.Chan
import Brick.Widgets.List


mkInitialState :: Chan Command -> Verbosity -> HMailState
mkInitialState chan verbosity =
  HMailState mempty [] chan (IsMailBoxesView mboxesView) verbosity
  where
    mboxesView = MailBoxesView
      (list ResBoxesList mempty 1)
