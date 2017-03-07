{-# language LambdaCase, Rank2Types #-}
module HMail.Brick.Util where

import Brick.Main
import Brick.Types

import Control.Lens
import Control.Monad
import Control.Monad.Extra


type EventHandler n e a =
  a -> BrickEvent n e -> EventM n (Next a)


embedH :: Lens' s a -> EventHandler n e a -> EventHandler n e s
embedH l h x e = fmap (into x) <$> h (outof x) e
  where
    into = flip (set l)
    outof = view l



{- (<:>) :: EventHandler n e a
  -> EventHandler n e a
  -> EventHandler n e a
(<:>) h1 h2 x e =
  h1 x e >>= maybe
    (h2 x e >>= maybe bad ret) ret
  where
    bad = pure Nothing
    ret = pure . Just


finalise :: EventHandler n e a
  -> a -> BrickEvent n e -> EventM n (Next a)
finalise h x e =
  h x e >>= maybe (continue x) continue
-}
