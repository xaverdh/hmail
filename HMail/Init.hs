{-# language TypeFamilies, Rank2Types #-}
module HMail.Init where

import HMail.Types

import DTypes
import DTypes.Collect
import DTypes.Instances.AlternativeInducesMonoid

import Control.Lens
import Control.Monad.Writer
import Data.Bifunctor

type Assemble d a = WriterT (d Maybe) IO a

fromLens :: Monoid s
  => ASetter s b a (Maybe c) -> c -> b
fromLens l x = mempty & l .~ Just x

{-
embed :: (d Maybe -> d' Maybe) -> Assemble d a -> Assemble d' a
embed f = mapWriterT . fmap . second $ f

embedImap :: Assemble DImapInit a -> Assemble DInit a
embedImap = embed fromImap

embedSmtp :: Assemble DSmtpInit a -> Assemble DInit a
embedSmtp = embed fromSmtp
-}
