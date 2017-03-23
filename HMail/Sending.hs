module HMail.Sending where

import HMail.Types
import HMail.Mail

import System.IO.Temp
import System.Process
import System.Exit
import qualified Data.ByteString as B

editorArgs :: FilePath -> [String]
editorArgs path = [path]

editor :: FilePath
editor = "/usr/bin/nano"

requestMail :: B.ByteString -> IO (Maybe B.ByteString)
requestMail initBs =
  withSystemTempFile "hmail-new-mail" $ \p fh -> do
    B.writeFile p initBs
    (_,_,_,ph) <- createProcess (proc editor (editorArgs p))
    exCode <- waitForProcess ph
    case exCode of
      ExitFailure i -> pure $ Nothing
      ExitSuccess -> Just <$> B.hGetContents fh

composeMail :: Mail -> IO (Maybe Mail)
composeMail mail =
  (deSerialise =<<)
  <$> requestMail (serialise mail)

