{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive.OAuth where

import           Control.Monad.IO.Class        (liftIO)
import           Data.ByteString               (ByteString)
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           Database
import           GoogleDrive.Types
import           Network.HTTP.Simple
import qualified Network.HTTP.Simple           as HTTP
import           Network.HTTP.Types.URI
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple

-- Handlers

authSuccessHandler :: Handler b GoogleDrive ()
authSuccessHandler = writeText "access tokens stored"

redirectHandler :: Config -> Handler b GoogleDrive ()
redirectHandler config = do
  code <- decode <$> getParam "code"
  res  <- getResponseBody <$> (liftIO . getToken config $ AuthCode code)
  storeAccessToken   $ accessToken res
  handleStoreRefresh $ refreshToken res
  redirect "/google-drive/auth-success"
  where
    decode             = decodeUtf8 . fromMaybe ""
    handleStoreRefresh = maybe (return 0) storeRefreshToken

signInHandler :: Config -> Handler b GoogleDrive ()
signInHandler = redirect . signInUrl


-- Requests

getToken :: Config -> AuthCode -> IO (HTTP.Response AuthResponse)
getToken config code = do
  baseReq <- parseRequest "POST https://www.googleapis.com/oauth2/v4/token"
  let formBody = tokenFormBody config code
      req      = setRequestBodyURLEncoded formBody baseReq
  httpJSON req


-- URI Helpers

tokenFormBody :: Config -> AuthCode -> [(ByteString, ByteString)]
tokenFormBody config (AuthCode x) =
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
