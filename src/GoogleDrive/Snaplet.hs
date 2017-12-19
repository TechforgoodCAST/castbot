{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GoogleDrive.Snaplet where

import           Control.Lens              (makeLenses, view)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.ByteString.Char8     (ByteString, pack, unpack)
import           Data.DateTime             (getCurrentTime)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import           Data.Text.Encoding        (decodeUtf8)
import           Database.Redis
import           Environment
import           GoogleDrive.Database
import           GoogleDrive.Request
import           GoogleDrive.Types
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

            -- routes that can only be triggered by internal HTTP client
            , ("/check-files",    get $ internalRoute checkNewFilesHandler)
            ]
  return $ GoogleDrive redis gDriveConfig
  where get  = method GET
        post = method POST


-- Auth Handlers

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


-- New Files Handlers

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
      else
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


-- Notifications Handlers

notifications :: Handler b GoogleDrive ()
notifications = do
  config <- view conf
  p      <- rqPostParams <$> getRequest
  if isFromSlack config p then
    handleSlackPayload $ slackPayload p
  else do
    modifyResponse $ setResponseStatus 401 "unauthorized"
    writeBS "unauthorized"

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


isFromSlack :: GDriveConfig -> Params -> Bool
isFromSlack config p = fromMaybe False $ do
  x <- M.lookup "token" p
  y <- safeHead x
  return $ y == slackToken config

slackPayload :: Params -> Maybe ByteString
slackPayload p = M.lookup "text" p >>= safeHead


-- Handler for routes only available to the internal HTTP Client

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
