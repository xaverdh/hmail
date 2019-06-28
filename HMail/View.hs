{-# language LambdaCase #-}
module HMail.View where

import HMail.Types
import HMail.Brick.EventH
import Control.Monad.Writer

tellView :: View -> EventH v ()
tellView = tell . Last . Just

