{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8            (pack, unpack)
import Data.Text.Encoding
import Database.Redis
import GoogleDrive.Types
import Prelude                          hiding (takeWhile)
import Snap.Snaplet.RedisDB
import System.Environment               (getEnv)
import System.Exit                      (exitFailure)
import Util                             (printFail)

setTokens :: AuthResponse -> Redis (Either Reply Status)
setTokens (AuthResponse at rt) = do
  maybe (return noOp) setRefreshToken rt
  setAccessToken at
  where
    noOp = Right Ok

setRefreshToken :: RefreshToken -> Redis (Either Reply Status)
setRefreshToken (RefreshToken x) = set "refresh_token" $ encodeUtf8 x

setAccessToken :: AccessToken -> Redis (Either Reply Status)
setAccessToken (AccessToken x)   = set "access_token" $ encodeUtf8 x

redisConnectInfo :: IO ConnectInfo
redisConnectInfo = parseCon <$> getEnv "REDIS_URL" >>= either printFail return
  where parseCon = parseOnly connectionParser . pack

connectionParser :: Parser ConnectInfo
connectionParser = do
  _    <- string "redis://h:"
  auth <- takeTill at
  _    <- char '@'
  host <- takeTill colon
  _    <- char ':'
  port <- decimal
  return $ defaultConnectInfo { connectHost = unpack host
                              , connectPort = PortNumber port
                              , connectAuth = Just auth
                              }
  where at    = (==) '@'
        colon = (==) ':'
