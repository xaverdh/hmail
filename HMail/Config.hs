{-# language GADTs, TypeOperators, FlexibleInstances #-}
-- GADTs, FlexibleInstances, FlexibleContexts, TemplateHaskell, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes #-}
module HMail.Config where

import HMail.Types

import DTypes
import DTypes.Collect
import DTypes.Instances.AlternativeInducesMonoid

import Control.Applicative
import Control.Monad.Writer

import System.Environment
import System.Directory
import System.FilePath

import Data.Monoid
import Text.Read (readMaybe)

import Options.Applicative as OA
import Options.Applicative.Builder as OB
import Options.Applicative.Help.Types (ParserHelp)

type Assemble a = WriterT (DImapInit Maybe) IO a

getConfigPaths :: IO [String]
getConfigPaths = do
  home <- getHomeDirectory
  pure $
    [ home </> ".config/hmailrc"
     ,home </> ".hmailrc"
     ,"/etc/hmailrc" ]


assembleConfig :: IO (Maybe ImapInit)
assembleConfig = fmap collect . execWriterT $
  assembleFromFiles
  >> assembleFromCmdline
  >> assembleDefaults

assembleFromFiles :: Assemble ()
assembleFromFiles = do
  -- paths <- getConfigPaths forM paths $ \path ->
  pure ()

-- parseConfig :: 

assembleFromCmdline :: Assemble ()
assembleFromCmdline = do
  args <- liftIO getArgs
  case execParserPure OB.defaultPrefs parser args of
    Success a ->  tell a
    Failure e -> liftIO $ showError e
  where
    showError :: ParserFailure ParserHelp -> IO ()
    showError e = putStrLn . fst
      $ renderFailure e ""
    
    conf = OB.defaultPrefs {
      prefDisambiguate = True
      ,prefShowHelpOnError = True
    }
    
    parser = info (helper <*> hmailOptions)
      (fullDesc
        <> header "hmail: a mutt-style mail client, written in haskell"
        -- <> progDescDoc (Just description)
        <> failureCode 1)
    
    hmailOptions :: OA.Parser (DImapInit Maybe)
    hmailOptions = DImapInit
      <$> genOpt (long "hostname" <> metavar "HOST"
        <> help "the host to connect to")
      <*> portOpt (long "port" <> metavar "PORT"
        <> help "the port to connect to")
      <*> genOpt (long "username" <> metavar "USER"
        <> help "the username of the account")
      <*> genOpt (long "password" <> metavar "PWD"
        <> help "the imap password")
    
    genOpt = optional . option str
    portOpt = optional . option
      (maybeReader $ fmap fromInteger . readMaybe)

assembleDefaults :: Assemble ()
assembleDefaults = pure ()

