{-# language ScopedTypeVariables, LambdaCase, OverloadedStrings #-}
module HMail.Config.Parser where

import HMail.Types

import DTypes.Instances.AlternativeInducesMonoid

import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import qualified Control.Exception as E

import System.IO
import System.Directory
import System.FilePath

import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.ByteString as B

import Data.Attoparsec.ByteString.Char8

getConfigPaths :: IO [String]
getConfigPaths = do
  home <- getHomeDirectory
  pure $
    [ home </> ".config/hmailrc"
     ,home </> ".hmailrc"
     ,"/etc/hmailrc" ]


readConfigs :: IO (DImapInit Maybe)
readConfigs = parseConfigs
  =<< mapMaybeM attempt
  =<< getConfigPaths
  where
    attempt :: FilePath -> IO (Maybe B.ByteString)
    attempt path = ignoreErrs <$> E.try (B.readFile path)
    
    ignoreErrs = \case
      Left (e::E.IOException) -> Nothing
      Right s -> Just s

parseConfigs :: [B.ByteString] -> IO (DImapInit Maybe)
parseConfigs = \case
  [] -> pure mempty
  bs:rst -> case parseOnly configP bs of
    Left err -> -- putStrLn (show err) >>
      parseConfigs rst
    Right val -> (val <>)
      <$> parseConfigs rst 

configP :: Parser (DImapInit Maybe)
configP = skipSpace
  *> (F.fold <$> (assignment `sepBy` skipSpace))
  <* skipSpace
  where
    fromHost x = mempty { d_imapHostname = Just x }
    fromPort x = mempty { d_imapPort = Just x }
    fromUName x = mempty { d_imapUsername = Just x }
    fromPwd x = mempty { d_imapPassword = Just x }
    
    assignment :: Parser (DImapInit Maybe)
    assignment = choice
      [ assign "hostname" (fromHost <$> configStr) 
       ,assign "port" (fromPort <$> decimal)
       ,assign "username" (fromUName <$> configStr) 
       ,assign "password" (fromPwd <$> configStr) ]
    
    assign key val =
      string key
      *> skipSpace
      *> char '='
      *> skipSpace
      *> val
    
    configStr :: Parser String
    configStr = quoted <|> unquoted
    
    quoted = char '"'
      *> many ( satisfy (/= '"')
            <|> ( char '\\' *> anyChar ) )
      <* char '"' 
    
    unquoted = anyChar `manyTill` (void space <|> endOfInput)
