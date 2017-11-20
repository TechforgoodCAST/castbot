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
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text.Encoding        (decodeUtf8)
import           Database
import           Database.Redis
import           Environment
import           GoogleDrive.Types
import           Network.HTTP.Simple
import           Network.HTTP.Types        (hAuthorization, renderQuery)
import           Snap.Core                 hiding (Request, Response)
import           Snap.Snaplet
import           Snap.Snaplet.RedisDB      (RedisDB, redisDBInit, runRedisDB)
import           System.Random             (randomRIO)
import           Util                      (safeHead)

data GoogleDrive =
  GoogleDrive {
    _db   :: Snaplet RedisDB
  , _conf :: GDriveConfig
  }

makeLenses ''GoogleDrive

-- Snaplet Init

gDriveInit :: SnapletInit b GoogleDrive
gDriveInit = makeSnaplet "google-drive" "google drive snaplet" Nothing $ do
  gDriveConfig <- loadGDriveConfig
  connInfo     <- liftIO loadRedisConnectInfo
  redis        <- nestSnaplet "db" db $ redisDBInit connInfo
  addRoutes [ ("/sign-in",        get signInHandler)
            , ("/redirect-auth",  get authRedirectHandler)
            , ("/auth-success",   get authSuccessHandler)
            , ("/notifications",  post notifications)

            -- routes that can only be triggered by internal http client
            , ("/check-files",    get $ internalRoute checkNewFilesHandler)
            ]
  return $ GoogleDrive redis gDriveConfig
  where get  = method GET
        post = method POST

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
    foldGifs                = either (const []) id

    handle config rfr = do
      now         <- liftIO getCurrentTime
      lastChecked <- foldLastChecked now <$> runRedisDB db (getLastChecked now)
      newFiles    <- liftIO $ checkNewFiles config rfr lastChecked
      if filesPresent newFiles then do
        gifs      <- foldGifs <$> runRedisDB db getGifs
        randomGif <- liftIO $ getRandomGif gifs
        liftIO . requestPostToSlack config $ SlackPost newFiles randomGif
        runRedisDB db $ setLastChecked now
        writeBS "Slack has been notified of new files added"
      else do
        runRedisDB db $ setLastChecked now
        writeBS "No new files"


getRandomGif :: [Gif] -> IO Gif
getRandomGif gifs = do
  let fallbackGif = Gif "https://media.giphy.com/media/cSVkEGjGsWz8k/giphy.gif"
  fromMaybe fallbackGif <$> randomItem gifs

randomItem :: [a] -> IO (Maybe a)
randomItem [] = return Nothing
randomItem xs = do
  i <- randomRIO (0, length xs - 1)
  return . Just $ xs !! i


notifications :: Handler b GoogleDrive ()
notifications = do
  config <- view conf
  p      <- rqPostParams <$> getRequest
  if isFromSlack config p then
    handleSlackPayload $ slackPayload p
  else do
    modifyResponse $ setResponseStatus 401 "unauthorized"
    writeBS "unauthorized"

isFromSlack :: GDriveConfig -> Params -> Bool
isFromSlack config p = fromMaybe False $ do
  x <- M.lookup "token" p
  y <- safeHead x
  return $ y == slackToken config

slackPayload :: Params -> Maybe ByteString
slackPayload p = M.lookup "text" p >>= safeHead

handleSlackPayload :: Maybe ByteString -> Handler b GoogleDrive ()
handleSlackPayload payload =
  case payload of
    Just "on"  -> notificationsOnHandler
    Just "off" -> notificationsOffHandler
    _          -> writeBS "Sorry I didn't quite get that, please enter either 'on' or 'off'"

notificationsOnHandler :: Handler b GoogleDrive ()
notificationsOnHandler = do
  runRedisDB db $ setPolling True
  writeBS "notifications switched on"

notificationsOffHandler :: Handler b GoogleDrive ()
notificationsOffHandler = do
  runRedisDB db $ setPolling False
  writeBS "notifications switched off"

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

checkNewFiles :: GDriveConfig -> RefreshToken -> DateTime -> IO Files
checkNewFiles config token lastChecked = newFiles lastChecked <$> requestFilesInFolder config token

newFiles :: DateTime -> Files -> Files
newFiles lastChecked (Files xs) = Files $ filter (\x -> createdDate x >= lastChecked) xs


-- HTTP Client Requests

requestPostToSlack :: GDriveConfig -> SlackPost -> IO (Response BL.ByteString)
requestPostToSlack config slackPost = do
  req  <- parseRequest $ "POST " <> unpack (webhookUrl config)
  let req' = setRequestBodyJSON slackPost req
  httpLBS req'

requestFilesInFolder :: GDriveConfig -> RefreshToken -> IO Files
requestFilesInFolder config refreshToken = do
  tkn     <- encodeToken <$> requestAccessToken config refreshToken
  baseReq <- parseRequest $ "GET https://www.googleapis.com/drive/v2/files" <> unpack filesQuery
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

filesQuery :: ByteString
filesQuery = renderQuery True
  [ ("q", Just $ proposalsFolderId <> " in parents and trashed = false") ]
  where
    proposalsFolderId = "\'0B_7-KMmA40dCcW9zZng1V2lxY0k\'"

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
