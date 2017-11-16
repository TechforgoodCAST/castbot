{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive.OAuth where

import           Control.Lens              (view)
import           Control.Monad.IO.Class    (liftIO)
import           Data.ByteString.Char8     (ByteString, unpack)
import           Data.DateTime
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import           Database                  (setTokens)
import           GoogleDrive.Types
import           Network.HTTP.Simple
import qualified Network.HTTP.Simple       as HTTP
import           Network.HTTP.Types.Header (hAuthorization)
import           Network.HTTP.Types.URI    (renderQuery)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.RedisDB      (runRedisDB)

-- Handlers

authSuccessHandler :: Handler b GoogleDrive ()
authSuccessHandler = writeText "access tokens stored"

redirectHandler :: Handler b GoogleDrive ()
redirectHandler = do
  config <- view conf
  code   <- decode <$> getParam "code"
  res    <- liftIO . requestAuthCredentials config $ AuthCode code
  runRedisDB db $ setTokens res
  redirect "/google-drive/auth-success"
  where
    decode = decodeUtf8 . fromMaybe ""


signInHandler :: Handler b GoogleDrive ()
signInHandler = view conf >>= (redirect . signInUrl)


-- Files

checkNewFiles :: Config -> RefreshToken -> IO Files
checkNewFiles config rft = requestFilesInFolder config rft >>= newFiles

newFiles :: Files -> IO Files
newFiles (Files xs) = do
  now <- getCurrentTime
  return . Files $ filter (\x -> addMinutes 1 (createdDate x) > now) xs


-- Requests

requestFilesInFolder :: Config -> RefreshToken -> IO Files
requestFilesInFolder config refreshToken = do
  tkn     <- encodeToken <$> requestAccessToken config refreshToken
  baseReq <- parseRequest $ "GET https://www.googleapis.com/drive/v2/files" <> unpack filesQuery
  let req = addRequestHeader hAuthorization ("Bearer " <> tkn) baseReq
  getResponseBody <$> httpJSON req

requestAuthCredentials :: Config -> AuthCode -> IO AuthResponse
requestAuthCredentials config code = do
  baseReq <- parseRequest "POST https://www.googleapis.com/oauth2/v4/token"
  let formBody = authCredentialsFormBody config code
      req      = setRequestBodyURLEncoded formBody baseReq
  getResponseBody <$> httpJSON req

requestAccessToken :: Config -> RefreshToken -> IO AccessToken
requestAccessToken config refreshToken = do
  baseReq <- parseRequest "POST https://www.googleapis.com/oauth2/v4/token"
  let formBody = accessTokenFormBody config refreshToken
      req      = setRequestBodyURLEncoded formBody baseReq
  getResponseBody <$> httpJSON req


-- URI Helpers

filesQuery :: ByteString
filesQuery = renderQuery True
  [ ("q", Just "\'1EMLrmlFkArl4txYQnMR62_gae4yjShux\' in parents and trashed = false") ]

accessTokenFormBody :: Config -> RefreshToken -> [(ByteString, ByteString)]
accessTokenFormBody config rft =
  [ ("client_id",     clientId config)
  , ("client_secret", clientSecret config)
  , ("refresh_token", encodeToken rft)
  , ("grant_type", "refresh_token")
  ]

authCredentialsFormBody :: Config -> AuthCode -> [(ByteString, ByteString)]
authCredentialsFormBody config ac =
  [ ("code",          encodeToken ac)
  , ("client_id",     clientId config)
  , ("client_secret", clientSecret config)
  , ("redirect_uri",  redirectUri config)
  , ("grant_type",    "authorization_code")
  ]

signInUrl :: Config -> ByteString
signInUrl config = baseUrl <> renderQuery True
   [ ("client_id",     Just $ clientId config)
   , ("redirect_uri",  Just $ redirectUri config)
   , ("scope",         Just scope)
   , ("access_type",   Just "offline")
   , ("response_type", Just "code")
  ]
  where
    baseUrl = "https://accounts.google.com/o/oauth2/v2/auth"
    scope   = "https://www.googleapis.com/auth/drive.readonly"
