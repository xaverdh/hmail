module HMail.Brick.Init (
  mkInitialState
, initView
) where

import HMail.Types
import Control.Concurrent.Chan

mkInitialState :: Chan Command -> Verbosity -> HMailState
mkInitialState chan verbosity =
  HMailState mempty [] chan verbosity

initView :: View
initView = IsMailBoxesView (MailBoxesView Nothing)
