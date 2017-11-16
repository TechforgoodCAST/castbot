{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive.OAuth where

import           Control.Lens           (view)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Database               (setTokens)
import           GoogleDrive.Types
import           Network.HTTP.Simple
import qualified Network.HTTP.Simple    as HTTP
import           Network.HTTP.Types.URI
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.RedisDB   (runRedisDB)

-- Handlers

authSuccessHandler :: Handler b GoogleDrive ()
authSuccessHandler = writeText "access tokens stored"

redirectHandler :: Handler b GoogleDrive ()
redirectHandler = do
  config <- view conf
  code   <- decode <$> getParam "code"
  res    <- getResponseBody <$> (liftIO . requestAuthCredentials config $ AuthCode code)
  runRedisDB db $ setTokens res
  redirect "/google-drive/auth-success"
  where
    decode = decodeUtf8 . fromMaybe ""


signInHandler :: Handler b GoogleDrive ()
signInHandler = view conf >>= (redirect . signInUrl)


-- Requests

requestAuthCredentials :: Config -> AuthCode -> IO (HTTP.Response AuthResponse)
requestAuthCredentials config code = do
  baseReq <- parseRequest "POST https://www.googleapis.com/oauth2/v4/token"
  let formBody = authCredentialsFormBody config code
      req      = setRequestBodyURLEncoded formBody baseReq
  httpJSON req

requestAccessToken :: Config -> RefreshToken -> IO (HTTP.Response AccessToken)
requestAccessToken config refreshToken = do
  baseReq <- parseRequest "POST https://www.googleapis.com/oauth2/v4/token"
  let formBody = accessTokenFormBody config refreshToken
      req      = setRequestBodyURLEncoded formBody baseReq
  httpJSON req


-- URI Helpers

accessTokenFormBody :: Config -> RefreshToken -> [(ByteString, ByteString)]
accessTokenFormBody config (RefreshToken x) =
  [ ("client_id", enc $ clientId config)
  , ("client_secret", enc $ clientSecret config)
  , ("refresh_token", enc x)
  , ("grant_type", "refresh_token")
  ]
  where enc = encodeUtf8

authCredentialsFormBody :: Config -> AuthCode -> [(ByteString, ByteString)]
authCredentialsFormBody config (AuthCode x) =
  [ ("code",          enc x)
  , ("client_id",     enc $ clientId config)
  , ("client_secret", enc $ clientSecret config)
  , ("redirect_uri",  enc $ redirectUri config)
  , ("grant_type",    "authorization_code")
  ]
  where enc = encodeUtf8

signInUrl :: Config -> ByteString
signInUrl config = baseUrl <> renderQuery True
   [ ("client_id",     Just . enc $ clientId config)
   , ("redirect_uri",  Just . enc $ redirectUri config)
   , ("scope",         Just $ enc scope)
   , ("access_type",   Just "offline")
   , ("response_type", Just "code")
  ]
  where
    enc     = encodeUtf8
    baseUrl = "https://accounts.google.com/o/oauth2/v2/auth"
    scope   = "https://www.googleapis.com/auth/drive.readonly"
