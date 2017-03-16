module HMail.Smtp (
  smtpThread
) where

import Network.HaskellNet.SMTP.SSL
-- import Network.HaskellNet.SMTP

import Control.Monad
import qualified Network.Mail.Mime as Mime


auth :: AuthType
  -> Username
  -> Password
  -> SMTPConnection -> IO Bool
auth authType
  (Username user)
  (Password pass)
  con = authenticate authType user pass con


smtpThread :: Mime.Mail -> Init -> IO ()
smtpThread mail init =
  doSMTPSSLWithSettings (init ^. smtpHostname) settings
    $ \con -> do
      success <- auth PLAIN
        (init ^. smtpUsername)
        (init ^. smtpPassword) con
      if not success
        then error "authentification failed"
        else sendMimeMail2 mail con
  where
    settings = defaultSettingsSMTPSSL {
        sslPort = init ^. stmpPort
        -- sslMaxLineLength
        -- sslLogToConsole
        -- sslDisableCertificateValidation
      }



