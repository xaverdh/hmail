{-# language LambdaCase #-}
module HMail.Brick.Util where

import Brick.Main
import Brick.Types
import Brick.Widgets.List

import Control.Lens
import Control.Monad
import Control.Monad.Extra

import Data.Vector

getSelected :: List n e -> Maybe e
getSelected lst =
  ((lst ^. listElementsL) !?)
  =<< (lst ^. listSelectedL)


