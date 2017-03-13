module HMail.Main where

import HMail.Brick (application)
import HMail.Brick.Init (mkInitialState)
import HMail.Imap (imapThread)
import HMail.State
import HMail.Types
import HMail.Config


import Brick.Main
import Brick.BChan
import Graphics.Vty (mkVty)
import Graphics.Vty.Config (defaultConfig)

import Control.Lens
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment

hmailMain :: DInit Maybe -> IO ()
hmailMain cmdline = do
  init <- maybe onErr id <$> assembleConfig cmdline
  bchan <- newBChan 10
  chan <- newChan
  forkIO $ imapThread init bchan chan
  finalState <- customMain
    (mkVty defaultConfig) (Just bchan)
    application (mkInitialState chan)
  forM (finalState ^. errLog) print
  pure ()
  where
    onErr = error "could not find config data"
