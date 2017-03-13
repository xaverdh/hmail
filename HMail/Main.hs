module HMail.Main where

import HMail.Brick (application)
import HMail.Brick.Init (mkInitialState)
import HMail.Imap (imapThread)
import HMail.State
import HMail.Types
import HMail.Init
import HMail.Config.Parser

import DTypes
import DTypes.Collect

import Brick.Main
import Brick.BChan
import Graphics.Vty (mkVty)
import Graphics.Vty.Config (defaultConfig)

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment

import Data.Monoid
import Data.Maybe


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


assembleConfig :: DInit Maybe -> IO (Maybe Init)
assembleConfig cmdline =
  fmap collectMaybe . execWriterT $
    smtpDefautToImap $ do
      getCmdLine -- ^ cmdline has highest priority
      readConfigs
      assembleDefaults
  where
    getCmdLine = tell cmdline :: Assemble DInit ()

assembleDefaults :: Assemble DInit ()
assembleDefaults = tell
  $ fromLens d_imapPortl (Port 993)
  <> fromLens d_smtpPortl (Port 587)


smtpDefautToImap :: Assemble DInit () -> Assemble DInit ()
smtpDefautToImap = censor $ \w ->
  let deflt imapLens smtpLens = smtpLens %~ f w imapLens
   in w & foldr (.) id
    [ deflt d_imapHostnamel d_smtpHostnamel
    , deflt d_imapPortl d_smtpPortl
    , deflt d_imapUsernamel d_smtpUsernamel
    , deflt d_imapPasswordl d_smtpPasswordl ]
  where
    f w l = case w ^? l of
      Just v -> case v of
        Just e -> const $ Just e
        Nothing -> id
      _ -> id


