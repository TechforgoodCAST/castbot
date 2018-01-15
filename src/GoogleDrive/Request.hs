{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive.Request where

import           Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy  as BL
import           Data.DateTime
import           Data.Monoid           ((<>))
import           GoogleDrive.Types
import           Network.HTTP.Simple
import           Network.HTTP.Types    (hAuthorization, renderQuery)

-- Check for Files Requests

checkNewFiles :: GDriveConfig -> ByteString -> RefreshToken -> DateTime -> IO Files
checkNewFiles config folderId token lastChecked =
  newFiles lastChecked <$> requestFilesInFolder config folderId token

newFiles :: DateTime -> Files -> Files
newFiles lastChecked (Files xs) = Files $ filter (\x -> createdDate x >= lastChecked) xs


-- HTTP Client Requests

requestPostToSlack :: GDriveConfig -> SlackPost -> IO (Response BL.ByteString)
requestPostToSlack config slackPost = do
  req  <- parseRequest $ "POST " <> unpack (webhookUrl config)
  let req' = setRequestBodyJSON slackPost req
  httpLBS req'

requestFilesInFolder :: GDriveConfig -> ByteString -> RefreshToken -> IO Files
requestFilesInFolder config folderId refreshToken = do
  let rawReq   = "GET https://www.googleapis.com/drive/v2/files" <> unpack (filesQuery folderId)
  tkn     <- encodeToken <$> requestAccessToken config refreshToken
  baseReq <- parseRequest rawReq
  let req = addRequestHeader hAuthorization ("Bearer " <> tkn) baseReq
  getResponseBody <$> httpJSON req

requestAccessToken :: GDriveConfig -> RefreshToken -> IO AccessToken
requestAccessToken config refreshToken = do
  baseReq <- parseRequest "POST https://www.googleapis.com/oauth2/v4/token"
  let formBody = accessTokenFormBody config refreshToken
      req      = setRequestBodyURLEncoded formBody baseReq
  getResponseBody <$> httpJSON req

requestAuthCredentials :: GDriveConfig -> AuthCode -> IO RefreshToken
requestAuthCredentials config code = do
  baseReq <- parseRequest "POST https://www.googleapis.com/oauth2/v4/token"
  let formBody = authCredentialsFormBody config code
      req      = setRequestBodyURLEncoded formBody baseReq
  getResponseBody <$> httpJSON req

authorizeInternalPoll :: GDriveConfig -> Request -> Request
authorizeInternalPoll config = addRequestHeader "polling_secret_key" $ pollingSecretKey config


-- URI Helpers

filesQuery :: ByteString -> ByteString
filesQuery folderId = renderQuery True
  [ ("q", Just $ folderId <> " in parents and trashed = false") ]
  -- where
  --   proposalsFolderId = "\'0B_7-KMmA40dCcW9zZng1V2lxY0k\'"

accessTokenFormBody :: GDriveConfig -> RefreshToken -> [(ByteString, ByteString)]
accessTokenFormBody config rft =
  [ ("client_id",     clientId config)
  , ("client_secret", clientSecret config)
  , ("refresh_token", encodeToken rft)
  , ("grant_type",    "refresh_token")
  ]

authCredentialsFormBody :: GDriveConfig -> AuthCode -> [(ByteString, ByteString)]
authCredentialsFormBody config code =
  [ ("code",          encodeToken code)
  , ("client_id",     clientId config)
  , ("client_secret", clientSecret config)
  , ("redirect_uri",  redirectUri config)
  , ("grant_type",    "authorization_code")
  ]

signInUrl :: GDriveConfig -> ByteString
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
