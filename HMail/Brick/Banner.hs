{-# language OverloadedStrings #-}
module HMail.Brick.Banner where

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Data.Semigroup

banner :: [String] -> Widget n
banner help = withAttr "banner"
  $ str "hmail-dev" <+>
  ( hCenter
    . hBox
    . map str
    . map (" "<>)
    $ help )

genericHelp :: [String]
genericHelp = [ "q:quit", "y:back" ]
