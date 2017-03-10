{-# language ScopedTypeVariables, LambdaCase #-}
module HMail.Config where

import HMail.Types
import HMail.Config.Parser

import DTypes
import DTypes.Collect

import Control.Applicative
import Control.Monad.Writer
import System.Environment
import Data.Monoid
import Text.Read (readMaybe)

import Options.Applicative as OA
import Options.Applicative.Builder as OB
import Options.Applicative.Help.Types (ParserHelp)

type Assemble a = WriterT (DImapInit Maybe) IO a

assembleConfig :: IO (Maybe ImapInit)
assembleConfig = fmap collect . execWriterT $ do
  assembleFromCmdline -- ^ cmdline has highest priority
  assembleFromFiles
  assembleDefaults

assembleFromFiles :: Assemble ()
assembleFromFiles = liftIO readConfigs >>= tell

assembleDefaults :: Assemble ()
assembleDefaults = tell $ mempty { d_imapPort = Just 993 }


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
        <> help "the username of the account to log into")
      <*> genOpt (long "password" <> metavar "PWD"
        <> help "the password for the imap account")
    
    genOpt = optional . option str
    portOpt = optional . option
      (maybeReader $ fmap fromInteger . readMaybe)

