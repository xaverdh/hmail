module HMail.Main where

import HMail.Brick (application)
import HMail.Brick.Init (mkInitialState,initView)
import HMail.Imap (imapThread)
import HMail.Types
import HMail.Init
import HMail.Config.Parser

import DTypes.Collect

import Brick.Main
import Brick.BChan
import Graphics.Vty (mkVty)
import Graphics.Vty.Config (defaultConfig)

import Control.Lens
import Control.Monad
import Control.Monad.Writer
import Control.Concurrent
import System.IO


hmailMain :: Verbosity -> DInit Maybe -> IO ()
hmailMain verbosity cmdline = do
  init <- maybe onErr id <$> assembleConfig cmdline
  bchan <- newBChan 10
  chan <- newChan
  void $ forkIO $ imapThread init bchan chan
  initVty <- mkVty defaultConfig
  (finalState,finalView) <- customMain
    initVty (mkVty defaultConfig)
    (Just bchan) application
    (mkInitialState chan verbosity,initView)
  void $ forM (finalState ^. errorLog) (hPutStrLn stderr)
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


