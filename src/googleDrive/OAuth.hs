{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive.OAuth where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           GoogleDrive.Types
import           Network.HTTP.Simple
import qualified Network.HTTP.Simple    as N
import           Snap.Core
import           Snap.Snaplet

redirectHandler :: Config -> Handler b GoogleDrive ()
redirectHandler config = do
  code <- decodeUtf8 . fromMaybe "" <$> getParam "code"
  res  <- liftIO $ tokenRequest config (AuthCode code)
  liftIO $ print res
  writeText $ "fetched code: " `T.append` code

tokenRequest :: Config -> AuthCode -> IO (N.Response AuthResponse)
tokenRequest config code = do
  baseReq <- parseRequest "POST https://www.googleapis.com/oauth2/v4/token"
  let formBody = tokenFormBody config code
      req      = setRequestBodyURLEncoded formBody baseReq
  httpJSON req

signInHandler :: Config -> Handler b GoogleDrive ()
signInHandler config = redirect $ signInQueryString config

tokenFormBody :: Config -> AuthCode -> [(ByteString, ByteString)]
tokenFormBody config (AuthCode x) =
  [ ("code",          enc x)
  , ("client_id",     enc $ clientId config)
  , ("client_secret", enc $ clientSecret config)
  , ("redirect_uri",  enc $ redirectUri config)
  , ("grant_type",    "authorization_code")
  ]
  where enc = encodeUtf8

signInQueryString :: Config -> ByteString
signInQueryString config = encodeUtf8 $
  mconcat [ baseUrl
          , "client_id=", clientId config
          , "&redirect_uri=", redirectUri config
          , "&scope=", scope
          , "&access_type=offline"
          , "&response_type=code"
          ]
  where
    baseUrl = "https://accounts.google.com/o/oauth2/v2/auth?"
    scope   = "https://www.googleapis.com/auth/drive.readonly"
