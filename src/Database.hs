{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Monad.IO.Class
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8            (ByteString, pack, unpack)
import Data.DateTime
import Data.Maybe                       (fromMaybe)
import Data.Text                        (Text)
import Data.Text.Encoding
import Database.Redis
import GoogleDrive.Types
import Snap.Snaplet.RedisDB
import System.Environment               (getEnv)
import System.Exit                      (exitFailure)
import Util                             (printFail)

-- key value getters and setters

getLastChecked :: DateTime -> Redis (Either Reply (Maybe DateTime))
getLastChecked now = parseTime now `deepMap` get "last_checked"
  where
    parseTime fallback = fromMaybe fallback . fromSqlString . unpack
    deepMap            = fmap . fmap . fmap

setLastChecked :: DateTime -> Redis (Either Reply Status)
setLastChecked = set "last_checked" . pack . toSqlString

getRefreshToken :: Redis (Either Reply (Maybe RefreshToken))
getRefreshToken = decodeToken `deepMap` get "refresh_token"
  where deepMap = fmap . fmap . fmap

setRefreshToken :: RefreshToken -> Redis (Either Reply Status)
setRefreshToken = set "refresh_token" . encodeToken


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
