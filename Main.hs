module Main where

import HMail.Types
import HMail.Init
import HMail.Main (hmailMain)

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F
import Text.Read (readMaybe)

import Options.Applicative as OA
import Options.Applicative.Builder as OB
-- import Options.Applicative.Help.Types (ParserHelp)

instance Semigroup a => Semigroup (OA.Parser a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (OA.Parser a) where
  mempty = pure mempty


main :: IO ()
main = parseCmdline


-- TODO
otherMain :: () -> IO ()
otherMain _ = pure ()

parseCmdline :: IO ()
parseCmdline = join $ customExecParser hmailPrefs parser
  where
    hmailPrefs = OB.defaultPrefs
      { prefDisambiguate = True
      , prefShowHelpOnError = True }

    parser = info (helper <*> hmailOptions)
      (fullDesc
        <> header
          "hmail: a mutt-style mail client, written in haskell"
        -- <> progDescDoc (Just description)
        <> failureCode 1)

hmailOptions :: OA.Parser (IO ())
hmailOptions = pure hmailMain
  <*> ( (fmap Verbosity . switch) (long "verbose" <> help "write debug output") )
  <*> ( hmailImapOptions <> hmailSmtpOptions )

hmailImapOptions :: OA.Parser (DInit Maybe)
hmailImapOptions = F.fold
  [ hostOpt d_imapHostnamel (long "imap-hostname" <> metavar "HOST"
    <> help "The (imap) host to connect to")
  , portOpt d_imapPortl (long "imap-port" <> metavar "PORT"
    <> value 993 <> help "The (imap) port to connect to"
    <> showDefault )
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

genOpt f l = fmap (maybe mempty $ fromLens l . f) . optional . OB.option str
hostOpt = genOpt Hostname
userOpt = genOpt Username
passOpt = genOpt Password
portOpt l = fmap (maybe mempty $ fromLens l . Port) . optional . OB.option
  (maybeReader $ fmap fromInteger . readMaybe)



