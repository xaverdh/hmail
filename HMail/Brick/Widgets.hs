{-# language OverloadedStrings #-}
module HMail.Brick.Widgets (
  loadingWidget
, errorWidget
) where
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border

loadingWidget = center . border $ txtWrap "LOADING..."

errorWidget = center . border $ txtWrap "an ERROR ocurred – sorry"

