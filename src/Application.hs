{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Application where

import           Client (pollForNewFiles)
import           Control.Arrow (second)
import           Control.Concurrent.Async (withAsync)
import           Control.Lens (makeLenses, view)
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.ByteString.Char8 (ByteString, pack, unpack)
import           Data.DateTime (getCurrentTime)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Text.Encoding (decodeUtf8)
import           Database
import           Database.Redis (Redis, Reply)
import           Environment
import           GoogleDrive.Request
import           GoogleDrive.Types
import           Snap.Core hiding (Request, Response)
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.Redis
import           Snap.Snaplet.RedisDB (RedisDB, redisDBInit, runRedisDB)
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.RedisSession
import           Snap.Util.FileServe (serveDirectory)
import           System.Random (randomRIO)
import           Util (safeHead)

data App =
  App {
    _db   :: Snaplet RedisDB
  , _sess :: Snaplet SessionManager
  , _auth :: Snaplet (AuthManager App)
  , _conf :: GDriveConfig
  }

makeLenses ''App

appInit :: SnapletInit App App
appInit = makeSnaplet "castmin-bot" "castmin slack bot" Nothing $ do
  connInfo     <- liftIO loadRedisConnectInfo
  redis        <- nestSnaplet "db" db $ redisDBInit connInfo
  let conn     = view snapletValue redis
  sess_        <- nestSnaplet "sess" sess $ initRedisSessionManager "site_key.txt" "_cookie" Nothing (Just 3600) conn
  auth_        <- nestSnaplet "auth" auth $ initRedisAuthManager sess conn
  gDriveConfig <- loadGDriveConfig
  addRoutes
    [ ("/", serveDirectory "static")
    , ("/", infoHandler)
    ]
  gDriveRoutes
    [ ("/sign-in",        get signInHandler)
    , ("/redirect-auth",  get authRedirectHandler)
    , ("/auth-success",   get authSuccessHandler)
    , ("/notifications",  post notifications)

    -- routes that can only be triggered by internal HTTP client
    , ("/check-files",    get $ internalRoute checkNewFilesHandler)
    ]
  return $ App redis sess_ auth_ gDriveConfig
  where get          = method GET
        post         = method POST
        gDriveRoutes = addRoutes . fmap (second $ dir "google-drive")

infoHandler :: Handler App App ()
infoHandler = writeBS "Visit /google-drive/sign-in to authenticate with google drive"

server :: IO ()
server = do
  p <- loadSnapServerPort
  let config = setPort p defaultConfig
  serveSnaplet config appInit


-- Runs the Snap server alongside the internal HTTP Client

app :: IO ()
app = pollForNewFiles `withAsync` const server


-- Auth Handlers

authSuccessHandler :: Handler b App ()
authSuccessHandler = writeText "authenication successful"

authRedirectHandler :: Handler b App ()
authRedirectHandler = do
  config <- view conf
  code   <- decode <$> getParam "code"
  res    <- liftIO . requestAuthCredentials config $ AuthCode code
  runRedis $ setRefreshToken res
  redirect "/google-drive/auth-success"
  where
    decode = decodeUtf8 . fromMaybe ""

signInHandler :: Handler b App ()
signInHandler = view conf >>= (redirect . signInUrl)


-- New Files Handlers

checkNewFilesHandler :: Handler b App ()
checkNewFilesHandler = do
  refreshToken <- runRedis getRefreshToken
  shouldPoll   <- fromResponse False <$> runRedis getPolling

  -- runs request to google drive api and posts to slack if polling is switched on
  if shouldPoll then
    either redisErr checkFiles refreshToken
  else
    writeBS "polling switched off"
  where
    checkFiles  = maybe noToken handleFiles
    redisErr    = writeBS . pack . show
    noToken     = writeBS "Cannot find refresh token, please reauthenticate"
    handleFiles = requestNewFilesHandler >=> postFilesToSlackHandler


requestNewFilesHandler :: RefreshToken -> Handler b App Files
requestNewFilesHandler rfr = do
  config      <- view conf
  now         <- liftIO getCurrentTime
  lastChecked <- fromResponse now <$> runRedis (getLastChecked now)
  folderId    <- fromResponse ""  <$> runRedis getProposalsFolderId
  liftIO $ checkNewFiles config folderId rfr lastChecked

postFilesToSlackHandler :: Files -> Handler b App ()
postFilesToSlackHandler newFiles =
  if filesPresent newFiles then do
    config    <- view conf
    now       <- liftIO getCurrentTime
    randomGif <- runRedis getGifs >>= liftIO <$> getRandomGif
    liftIO . requestPostToSlack config $ SlackPost newFiles randomGif
    runRedis $ setLastChecked now
    writeBS "Slack has been notified of new files added"
  else
    writeBS "No new files"
  where
    filesPresent (Files xs) = not $ null xs


getRandomGif :: Either Reply [Gif] -> IO Gif
getRandomGif = randomGif' . foldGifs
  where foldGifs = either (const []) id

randomGif' :: [Gif] -> IO Gif
randomGif' gifs = do
  let fallbackGif = Gif "https://media.giphy.com/media/cSVkEGjGsWz8k/giphy.gif"
  fromMaybe fallbackGif <$> randomItem gifs

randomItem :: [a] -> IO (Maybe a)
randomItem [] = return Nothing
randomItem xs = do
  i <- randomRIO (0, length xs - 1)
  return . Just $ xs !! i


-- Notifications Handlers

notifications :: Handler b App ()
notifications = do
  config <- view conf
  p      <- rqPostParams <$> getRequest
  if isFromSlack config p then
    handleSlackPayload $ slackPayload p
  else do
    modifyResponse $ setResponseStatus 401 "unauthorized"
    writeBS "unauthorized"

handleSlackPayload :: Maybe ByteString -> Handler b App ()
handleSlackPayload payload =
  case payload of
    Just "on"  -> notificationsOnHandler
    Just "off" -> notificationsOffHandler
    _          -> writeBS "Sorry I didn't quite get that, please enter either 'on' or 'off'"

notificationsOnHandler :: Handler b App ()
notificationsOnHandler = do
  runRedis $ setPolling True
  writeBS "notifications switched on"

notificationsOffHandler :: Handler b App ()
notificationsOffHandler = do
  runRedis $ setPolling False
  writeBS "notifications switched off"


isFromSlack :: GDriveConfig -> Params -> Bool
isFromSlack config p = fromMaybe False $ do
  x <- M.lookup "token" p
  y <- safeHead x
  return $ y == slackToken config

slackPayload :: Params -> Maybe ByteString
slackPayload p = M.lookup "text" p >>= safeHead


-- Handler for routes only available to the internal HTTP Client

internalRoute :: Handler b App () -> Handler b App ()
internalRoute continue = do
  secret  <- pollingSecretKey <$> view conf
  secret' <- headerSecret <$> getRequest
  if secret == secret' then
    continue
  else do
    modifyResponse $ setResponseStatus 401 "unauthorized"
    writeBS "unauthorized"
  where
    headerSecret = fromMaybe "" . getHeader "polling_secret_key" . rqHeaders


-- Helper to run redis in handler context

runRedis :: Redis a -> Handler b App a
runRedis = runRedisDB db
