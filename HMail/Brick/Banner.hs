module HMail.Brick.Banner where

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Data.Monoid

banner :: [String] -> Widget n
banner help = hCenter
  . hBox
  . map str
  . map (" "<>)
  $ "hmail-dev" : help

genericHelp :: [String]
genericHelp = [ "q: quit", "y: back" ]

