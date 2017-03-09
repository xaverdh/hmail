module HMail.Brick.Banner where

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Data.Monoid

banner :: [String] -> Widget n
banner help = str "hmail-dev" <+>
  ( hCenter
    . hBox
    . map str
    . map (" "<>)
    $ help )

genericHelp :: [String]
genericHelp = [ "q:quit", "y:back" ]
