module HMail.Brick.Init (
  mkInitialState
, initView
) where

import HMail.Types
import Control.Concurrent.Chan
import Brick.Widgets.List


mkInitialState :: Chan Command -> Verbosity -> HMailState
mkInitialState chan verbosity =
  HMailState mempty [] chan verbosity

initView :: View
initView = IsMailBoxesView mboxesView
  where
    mboxesView = MailBoxesView
      (list ResBoxesList mempty 1)
