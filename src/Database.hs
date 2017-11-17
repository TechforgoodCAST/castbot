{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8            (pack, unpack)
import Data.Text.Encoding
import Database.Redis
import GoogleDrive.Types
import Snap.Snaplet.RedisDB
import System.Environment               (getEnv)
import System.Exit                      (exitFailure)
import Util                             (printFail)

setRefreshToken :: RefreshToken -> Redis (Either Reply Status)
setRefreshToken = set "refresh_token" . encodeToken

setAccessToken :: AccessToken -> Redis (Either Reply Status)
setAccessToken  = set "access_token" . encodeToken


-- Connection Utils

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
