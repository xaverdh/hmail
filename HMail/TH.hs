module HMail.TH where

import Control.Lens
import Language.Haskell.TH

myMakeLenses :: Name -> DecsQ
myMakeLenses = makeLensesWith
  ( lensRules & lensField .~ mappingNamer f )
  where
    f name = [name ++ "l"]

