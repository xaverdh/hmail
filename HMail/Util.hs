module HMail.Util where

import qualified Data.Text as T

showT :: Show a => a -> T.Text
showT = T.pack . show
