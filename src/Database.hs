{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Database where

import Control.Monad.IO.Class
import Data.ByteString.Char8         (pack)
import Data.Int                      (Int64)
import Data.Monoid                   ((<>))
import GoogleDrive.Types
import Snap.Snaplet.PostgresqlSimple
import System.Environment            (lookupEnv)
import System.Exit                   (exitFailure)
import Text.RawString.QQ

getRefreshToken :: HasPostgres m => m [RefreshToken]
getRefreshToken = query_ getAccessTokenQuery

getAccessToken :: HasPostgres m => m [AccessToken]
getAccessToken = query_ getAccessTokenQuery

storeAccessToken :: HasPostgres m => AccessToken -> m Int64
storeAccessToken = execute storeAccessTokenQuery

storeRefreshToken :: HasPostgres m => RefreshToken -> m Int64
storeRefreshToken = execute storeRefreshTokenQuery

pgsConfig :: IO PGSConfig
pgsConfig = lookupEnv "DATABASE_URL" >>= maybe exit setupConfig
  where
    setupConfig = return . pgsDefaultConfig . pack
    exit        = putStrLn errMsg >> exitFailure
    errMsg      = "Please set DATABASE_URL env var"

createTables :: Query
createTables = [r|
CREATE TABLE access_token (
  id SERIAL PRIMARY KEY,
  token VARCHAR NOT NULL,
  timestamp timestamp DEFAULT current_timestamp
);
CREATE TABLE refresh_token (
  id SERIAL PRIMARY KEY,
  token VARCHAR NOT NULL,
  timestamp timestamp DEFAULT current_timestamp
);|]

getRefreshTokenQuery :: Query
getRefreshTokenQuery = "SELECT * FROM refresh_token;"

getAccessTokenQuery :: Query
getAccessTokenQuery = "SELECT * FROM access_token;"

storeAccessTokenQuery :: Query
storeAccessTokenQuery = "DELETE FROM access_token; " <> insertAccessTokenQuery

storeRefreshTokenQuery :: Query
storeRefreshTokenQuery = "DELETE FROM refresh_token; " <> insertRefreshTokenQuery

insertAccessTokenQuery :: Query
insertAccessTokenQuery  = "INSERT INTO access_token (token) VALUES (?);"

insertRefreshTokenQuery :: Query
insertRefreshTokenQuery = "INSERT INTO refresh_token (token) VALUES (?);"
