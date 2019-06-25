{-# language LambdaCase #-}
module HMail.View where

import HMail.Types


fromMailBoxesView :: Show n => View n -> MailBoxesView n
fromMailBoxesView = \case
  IsMailBoxesView v -> v
  other -> error $ "invalid internal state: wrong view; expected MailBoxes, got: " <> show other

fromMailBoxView :: Show n => View n -> MailBoxView n
fromMailBoxView = \case
  IsMailBoxView v -> v
  other -> error $ "invalid internal state: wrong view; expected MailBox, got: " <> show other

fromMailView :: Show n => View n -> MailView
fromMailView = \case
  IsMailView v -> v
  other -> error $ "invalid internal state: wrong view; expected MailView, got: " <> show other

{-
activeMailBoxView :: _
activeMailBoxView = to fromMailBoxView . activeView
-}

