module Main where

import HMail.Brick (application)
import HMail.Brick.Init (mkInitialState)
import HMail.Imap (imapThread)
import HMail.State
import HMail.Types


import Brick.Main
import Brick.BChan
import Graphics.Vty (mkVty)
import Graphics.Vty.Config (defaultConfig)

import Control.Lens
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  bchan <- newBChan 10
  chan <- newChan
  forkIO $ imapThread (mkImapInit args) bchan chan
  finalState <- customMain
    (mkVty defaultConfig) (Just bchan)
    application (mkInitialState chan)
  forM (finalState ^. errLog) print
  pure ()

mkImapInit :: [String] -> ImapInit
mkImapInit [host,port,uname,pass] =
  ImapInit {
    _imapHostname = host
   ,_imapPort = fromInteger $ read port
   ,_imapUsername = uname
   ,_imapPassword = pass
  }

