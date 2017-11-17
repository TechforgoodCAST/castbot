{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GoogleDrive where

import           Control.Lens              (makeLenses, view)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.ByteString.Char8     (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy      as BL
import           Data.DateTime             (addMinutes, getCurrentTime)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import           Data.Text.Encoding        (decodeUtf8)
import           Database                  (getRefreshToken, redisConnectInfo,
                                            setAccessToken, setRefreshToken)
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

            -- routes that can only be triggered by internal http client
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
         <*> mtLookup "INTERNAL_ROUTE_SECRET"
         <*> mtLookup "WEBHOOK_URL"
  where mtLookup x = pack <$> (MaybeT $ lookupEnv x)

loadGDriveConfig :: MonadIO m => m Config
loadGDriveConfig =
  getGDriveConfig >>= maybe fail return
  where fail = liftIO $ printFail msg
        msg  = "please set CLIENT_ID, CLIENT_SECRET, REDIRECT_URI & WEBHOOK_URL env vars"


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
  config <- view conf
  rfr    <- runRedisDB db getRefreshToken
  either redisErr (maybeFiles config) rfr
  where
    maybeFiles config       = maybe noToken $ handle config
    redisErr                = writeBS . pack . show
    noToken                 = writeBS "Cannot find refresh token, please reauthenticate"
    filesPresent (Files xs) = not $ null xs

    handle config rfr = do
      fx <- liftIO $ checkNewFiles config rfr
      if filesPresent fx then do
        liftIO $ requestPostToSlack config fx
        writeBS "Slack has been notified of new files added"
      else
        writeBS "No new files"

internalRoute :: Handler b GoogleDrive () -> Handler b GoogleDrive ()
internalRoute handler = do
  secret  <- internalRouteSecret <$> view conf
  secret' <- headerSecret <$> getRequest
  if   secret == secret'
  then handler
  else writeBS "unauthorized"
  where
    headerSecret = fromMaybe "" . getHeader "internal_route_secret" . rqHeaders


-- Files

checkNewFiles :: Config -> RefreshToken -> IO Files
checkNewFiles config rft = requestFilesInFolder config rft >>= newFiles

newFiles :: Files -> IO Files
newFiles (Files xs) = do
  now <- getCurrentTime
  return . Files $ filter (\x -> addMinutes 1000000 (createdDate x) > now) xs


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

authorizeInternal :: Config -> Request -> Request
authorizeInternal config = addRequestHeader "internal_route_secret" $ internalRouteSecret config


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
