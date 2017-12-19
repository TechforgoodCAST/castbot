{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive.Database where

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
import Util                             (deepMap)

-- key value getters and setters

getPolling :: Redis (Either Reply (Maybe Bool))
getPolling = (read . unpack) `deepMap` get "polling"

setPolling :: Bool -> Redis (Either Reply Status)
setPolling = set "polling" . pack . show

getLastChecked :: DateTime -> Redis (Either Reply (Maybe DateTime))
getLastChecked fallback = parseTime fallback `deepMap` get "last_checked"
  where parseTime t = fromMaybe t . fromSqlString . unpack

setLastChecked :: DateTime -> Redis (Either Reply Status)
setLastChecked = set "last_checked" . pack . toSqlString

getRefreshToken :: Redis (Either Reply (Maybe RefreshToken))
getRefreshToken = decodeToken `deepMap` get "refresh_token"

setRefreshToken :: RefreshToken -> Redis (Either Reply Status)
setRefreshToken = set "refresh_token" . encodeToken

getGifs :: Redis (Either Reply [Gif])
getGifs = decodeGif `deepMap` smembers "gifs"
  where decodeGif = Gif . decodeUtf8

addGifs :: [Gif] -> Redis (Either Reply Integer)
addGifs = sadd "gifs" . map enc
  where enc (Gif url) = encodeUtf8 url


-- Redis Connection Parser

parseRedisConnection :: String -> Either String ConnectInfo
parseRedisConnection = parseOnly connectionParser . pack

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
