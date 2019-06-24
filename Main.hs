module Main where

import HMail.Types
import HMail.Init
import HMail.Main (hmailMain)

import DTypes
import DTypes.Collect
import DTypes.Instances.AlternativeInducesMonoid


import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment
import System.Exit
import Data.Semigroup
import Data.Maybe
import qualified Data.Foldable as F
import Text.Read (readMaybe)

import Options.Applicative as OA
import Options.Applicative.Builder as OB
import Options.Applicative.Help.Types (ParserHelp)

instance Semigroup a => Semigroup (OA.Parser a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (OA.Parser a) where
  mempty = pure mempty


main :: IO ()
main = do
  args <- liftIO getArgs
  parseCmdline args


-- TODO
otherMain :: () -> IO ()
otherMain _ = pure ()

parseCmdline :: [String] -> IO ()
parseCmdline args = do
  case execParserPure OB.defaultPrefs parser args of
    Success a -> a
    Failure e -> showError e >> exitWith (ExitFailure 1)
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
        <> header
          "hmail: a mutt-style mail client, written in haskell"
        -- <> progDescDoc (Just description)
        <> failureCode 1)

hmailOptions :: OA.Parser (IO ())
hmailOptions = do
  liftA otherMain otherOptions
  <|> fmap hmailMain ( hmailImapOptions <> hmailSmtpOptions )

-- TODO
otherOptions = empty


hmailImapOptions :: OA.Parser (DInit Maybe)
hmailImapOptions = F.fold
  [ hostOpt d_imapHostnamel (long "imap-hostname" <> metavar "HOST"
    <> help "The (imap) host to connect to")
  , portOpt d_imapPortl (long "imap-port" <> metavar "PORT"
    <> help "The (imap) port to connect to")
  , userOpt d_imapUsernamel (long "imap-username" <> metavar "USER"
    <> help "The username of the (imap) account to log into")
  , passOpt d_imapPasswordl (long "imap-password" <> metavar "PWD"
    <> help "The password for the (imap) account") ]

hmailSmtpOptions :: OA.Parser (DInit Maybe)
hmailSmtpOptions = F.fold
  [ hostOpt d_smtpHostnamel (long "smtp-hostname" <> metavar "HOST"
    <> help "The (smtp) host to connect to")
  , portOpt d_smtpPortl (long "smtp-port" <> metavar "PORT"
    <> help "The (smtp) port to connect to")
  , userOpt d_smtpUsernamel (long "smtp-username" <> metavar "USER"
    <> help "The username of the (smtp) account to log into")
  , passOpt d_smtpPasswordl (long "smtp-password" <> metavar "PWD"
    <> help "The password for the (smtp) account") ]

genOpt f l = fmap (maybe mempty $ fromLens l . f) . optional . option str
hostOpt = genOpt Hostname
userOpt = genOpt Username
passOpt = genOpt Password
portOpt l = fmap (maybe mempty $ fromLens l . Port) . optional . option
  (maybeReader $ fmap fromInteger . readMaybe)



