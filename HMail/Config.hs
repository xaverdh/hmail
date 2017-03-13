{-# language ScopedTypeVariables, LambdaCase #-}
module HMail.Config where

import HMail.Types
import HMail.Init
import HMail.Config.Parser

import DTypes
import DTypes.Collect

import Control.Lens
import Control.Applicative
import Control.Monad.Writer
import System.Environment
import Data.Monoid
import Data.Bifunctor
import Data.Maybe

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
assembleDefaults = tell $ do
  ( mempty & d_imapPortl .~ Just (Port 993) )
  <> ( mempty & d_smtpPortl .~ Just (Port 587) )


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


