{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GoogleDrive where

import           Control.Lens              (makeLenses, view)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.ByteString.Char8     (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy      as BL
import           Data.DateTime
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import           Data.Text.Encoding        (decodeUtf8)
import           Database
import           Database.Redis
import           GoogleDrive.Types
import           Network.HTTP.Simple
import           Network.HTTP.Types        (hAuthorization, renderQuery)
import           Snap.Core                 hiding (Request, Response)
import           Snap.Snaplet
import           Snap.Snaplet.RedisDB      (RedisDB, redisDBInit, runRedisDB)
import           System.Environment        (lookupEnv)
import           System.Exit               (exitFailure)
import           Util                      (printFail)

data GoogleDrive =
  GoogleDrive {
    _db   :: Snaplet RedisDB
  , _conf :: Config
  }

makeLenses ''GoogleDrive

-- Snaplet Init

gDriveInit :: SnapletInit b GoogleDrive
gDriveInit = makeSnaplet "google-drive" "google drive snaplet" Nothing $ do
  gDriveConfig <- loadGDriveConfig
  connInfo     <- liftIO redisConnectInfo
  redis        <- nestSnaplet "db" db $ redisDBInit connInfo
  addRoutes [ ("/sign-in",        get signInHandler)
            , ("/redirect-auth",  get authRedirectHandler)
            , ("/auth-success",   get authSuccessHandler)
            , ("/polling/on",     get pollingOnHandler)
            , ("/polling/off",    get pollingOffHandler)

            -- polling routes that can only be triggered by internal http client
            , ("/check-files",    get $ internalRoute checkNewFilesHandler)
            ]
  return $ GoogleDrive redis gDriveConfig
  where get = method GET


-- Config

getGDriveConfig :: MonadIO m => m (Maybe Config)
getGDriveConfig = liftIO . runMaybeT $
  Config <$> mtLookup "CLIENT_ID"
         <*> mtLookup "CLIENT_SECRET"
         <*> mtLookup "REDIRECT_URI"
         <*> mtLookup "POLLING_SECRET_KEY"
         <*> mtLookup "WEBHOOK_URL"
  where mtLookup x = pack <$> (MaybeT $ lookupEnv x)

loadGDriveConfig :: MonadIO m => m Config
loadGDriveConfig =
  getGDriveConfig >>= maybe fail return
  where fail = liftIO $ printFail msg
        msg  = mconcat [ "please set CLIENT_ID, "
                       , "CLIENT_SECRET, "
                       , "REDIRECT_URI, "
                       , "POLLING_SECRET_KEY, "
                       , "& WEBHOOK_URL env vars"
                       ]


-- Handlers

authSuccessHandler :: Handler b GoogleDrive ()
authSuccessHandler = writeText "authenication successful"

authRedirectHandler :: Handler b GoogleDrive ()
authRedirectHandler = do
  config <- view conf
  code   <- decode <$> getParam "code"
  res    <- liftIO . requestAuthCredentials config $ AuthCode code
  runRedisDB db $ setRefreshToken res
  redirect "/google-drive/auth-success"
  where
    decode = decodeUtf8 . fromMaybe ""

signInHandler :: Handler b GoogleDrive ()
signInHandler = view conf >>= (redirect . signInUrl)

checkNewFilesHandler :: Handler b GoogleDrive ()
checkNewFilesHandler = do
  config       <- view conf
  refreshToken <- runRedisDB db getRefreshToken
  shouldPoll   <- foldPolling <$> runRedisDB db getPolling

  -- runs request to google drive api and posts to slack if polling is switched on
  if   shouldPoll
  then either redisErr (maybeCheckFiles config) refreshToken
  else writeBS "polling switched off"

  where
    maybeCheckFiles config  = maybe noToken $ handle config
    redisErr                = writeBS . pack . show
    noToken                 = writeBS "Cannot find refresh token, please reauthenticate"
    filesPresent (Files xs) = not $ null xs
    foldLastChecked now     = either (const now) (fromMaybe now)
    foldPolling             = either (const False) (fromMaybe False)

    handle config rfr = do
      now         <- liftIO getCurrentTime
      lastChecked <- foldLastChecked now <$> runRedisDB db (getLastChecked now)
      newFiles    <- liftIO $ checkNewFiles config rfr lastChecked
      if filesPresent newFiles then do
        liftIO $ requestPostToSlack config newFiles
        runRedisDB db $ setLastChecked now
        writeBS "Slack has been notified of new files added"
      else
        writeBS "No new files"


pollingOnHandler :: Handler b GoogleDrive ()
pollingOnHandler = do
  runRedisDB db $ setPolling True
  writeBS "polling switched on"

pollingOffHandler :: Handler b GoogleDrive ()
pollingOffHandler = do
  runRedisDB db $ setPolling False
  writeBS "polling switched off"

internalRoute :: Handler b GoogleDrive () -> Handler b GoogleDrive ()
internalRoute handler = do
  secret  <- pollingSecretKey <$> view conf
  secret' <- headerSecret <$> getRequest
  if   secret == secret'
  then handler
  else do
    modifyResponse $ setResponseStatus 401 "unauthorized"
    writeBS "unauthorized"
  where
    headerSecret = fromMaybe "" . getHeader "polling_secret_key" . rqHeaders


-- Files

checkNewFiles :: Config -> RefreshToken -> DateTime -> IO Files
checkNewFiles config token lastChecked = newFiles lastChecked <$> requestFilesInFolder config token

newFiles :: DateTime -> Files -> Files
newFiles lastChecked (Files xs) = Files $ filter (\x -> createdDate x >= lastChecked) xs


-- Requests

requestPostToSlack :: Config -> Files -> IO (Response BL.ByteString)
requestPostToSlack config files = do
  req  <- parseRequest $ "POST " <> unpack (webhookUrl config)
  let req' = setRequestBodyJSON files req
  httpLBS req'

requestFilesInFolder :: Config -> RefreshToken -> IO Files
requestFilesInFolder config refreshToken = do
  tkn     <- encodeToken <$> requestAccessToken config refreshToken
  baseReq <- parseRequest $ "GET https://www.googleapis.com/drive/v2/files" <> unpack filesQuery
  let req = addRequestHeader hAuthorization ("Bearer " <> tkn) baseReq
  getResponseBody <$> httpJSON req

requestAccessToken :: Config -> RefreshToken -> IO AccessToken
requestAccessToken config refreshToken = do
  baseReq <- parseRequest "POST https://www.googleapis.com/oauth2/v4/token"
  let formBody = accessTokenFormBody config refreshToken
      req      = setRequestBodyURLEncoded formBody baseReq
  getResponseBody <$> httpJSON req

requestAuthCredentials :: Config -> AuthCode -> IO RefreshToken
requestAuthCredentials config code = do
  baseReq <- parseRequest "POST https://www.googleapis.com/oauth2/v4/token"
  let formBody = authCredentialsFormBody config code
      req      = setRequestBodyURLEncoded formBody baseReq
  getResponseBody <$> httpJSON req

authorizeInternalPoll :: Config -> Request -> Request
authorizeInternalPoll config = addRequestHeader "polling_secret_key" $ pollingSecretKey config


-- URI Helpers

filesQuery :: ByteString
filesQuery = renderQuery True
  [ ("q", Just "\'1EMLrmlFkArl4txYQnMR62_gae4yjShux\' in parents and trashed = false") ]

accessTokenFormBody :: Config -> RefreshToken -> [(ByteString, ByteString)]
accessTokenFormBody config rft =
  [ ("client_id",     clientId config)
  , ("client_secret", clientSecret config)
  , ("refresh_token", encodeToken rft)
  , ("grant_type",    "refresh_token")
  ]

authCredentialsFormBody :: Config -> AuthCode -> [(ByteString, ByteString)]
authCredentialsFormBody config code =
  [ ("code",          encodeToken code)
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
