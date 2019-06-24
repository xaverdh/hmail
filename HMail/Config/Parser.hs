{-# language ScopedTypeVariables, LambdaCase, OverloadedStrings #-}
{-# language FlexibleContexts #-}
module HMail.Config.Parser where

import HMail.Types
import HMail.Init

import DTypes.Instances.AlternativeInducesMonoid

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Writer
import qualified Control.Exception as E

import System.IO
import System.Directory
import System.FilePath

import qualified Data.Foldable as F
import Data.Semigroup
import qualified Data.ByteString as B

import Text.Parser.Combinators
import Data.Attoparsec.ByteString.Char8 
  hiding (choice,sepBy1,manyTill)

getConfigPaths :: IO [String]
getConfigPaths = do
  home <- getHomeDirectory
  pure $
    [ home </> ".config/hmailrc"
     ,home </> ".hmailrc"
     ,"/etc/hmailrc" ]


readConfigs :: Assemble DInit ()
readConfigs = parseConfigs
  =<< liftIO
    ( mapMaybeM attempt
      =<< getConfigPaths )
  where
    attempt :: FilePath -> IO (Maybe B.ByteString)
    attempt path = ignoreErrs <$> E.try (B.readFile path)
    
    ignoreErrs = \case
      Left (e::E.IOException) -> Nothing
      Right s -> Just s

parseConfigs :: [B.ByteString] -> Assemble DInit ()
parseConfigs = \case
  [] -> pure mempty
  bs:rst -> case parseOnly configP bs of
    Left err -> -- putStrLn (show err) >>
      parseConfigs rst
    Right init -> tell init >> parseConfigs rst

configP :: Parser (DInit Maybe)
configP = between skipSpace skipSpace
  ( F.fold <$> ( parts `sepBy1` skipSpace ) )
  where
    parts = choice [ imapSection, smtpSection ]

imapSection :: Parser (DInit Maybe)
imapSection = section "imap"
  [ hostnameP d_imapHostnamel
  , portP d_imapPortl
  , usernameP d_imapUsernamel
  , passwordP d_imapPasswordl ]

smtpSection :: Parser (DInit Maybe)
smtpSection = section "smtp"
  [ hostnameP d_smtpHostnamel
  , portP d_smtpPortl
  , usernameP d_smtpUsernamel
  , passwordP d_smtpPasswordl ]

section :: Monoid m => B.ByteString -> [Parser m] -> Parser m
section name parsers = 
  between (char '[') (char ']') (stringCI name)
  *> skipSpace *> block parsers

block :: Monoid m => [Parser m] -> Parser m
block parsers = F.fold
  <$> ( choice parsers `sepBy1` skipSpace )


hostnameP f = assignP "hostname"
  (fromLens f . Hostname <$> configStr)
portP f = assignP "port"
  (fromLens f . Port . fromIntegral <$> decimal)
usernameP f = assignP "username"
  (fromLens f . Username <$> configStr)
passwordP f = assignP "password"  
  (fromLens f . Password <$> configStr)

assignP :: B.ByteString -> Parser a -> Parser a
assignP key valp =
  string key
  *> skipSpace
  *> char '='
  *> skipSpace
  *> valp

configStr :: Parser String
configStr = quoted <|> unquoted where
  quoted = char '"'
    *> many ( satisfy (/= '"')
          <|> ( char '\\' *> anyChar ) )
    <* char '"' 
  
  unquoted = anyChar `manyTill` (void space <|> endOfInput)
  
