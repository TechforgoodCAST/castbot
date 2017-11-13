{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson
import Data.ByteString.Lazy      (ByteString)
import Data.Foldable             (fold)
import Data.Monoid               ((<>))
import Data.Text
import GHC.Generics              (Generic)
import GoogleDrive.OAuth
import GoogleDrive.Types
import Network.HTTP.Simple
import Snap.Core                 (Method (..), method, writeLBS)
import Snap.Snaplet
import System.Environment
import System.Exit               (exitFailure)

sendToSlackHandler :: Handler b GoogleDrive ()
sendToSlackHandler = method GET $ do
  res <- liftIO sendToSlack
  writeLBS $ fold res

sendToSlack :: IO (Response ByteString)
sendToSlack = do
  url  <- liftIO $ getEnv "WEBHOOK_URL"
  req  <- parseRequest $ "POST " <> url
  let req' = setRequestBodyJSON (SlackPost "hello from snap") req
  httpLBS req'

getConfig :: MonadIO m => m (Maybe Config)
getConfig = liftIO . runMaybeT $ do
  cId <- MaybeT $ lookupEnv "CLIENT_ID"
  cSc <- MaybeT $ lookupEnv "CLIENT_SECRET"
  return $ Config (pack cId) (pack cSc) redirectUri
  where
    redirectUri = "http://localhost:8000/google-drive/redirect-auth"

loadConfig :: MonadIO m => m Config
loadConfig =
  getConfig >>= maybe (liftIO $ putStrLn errMsg >> exitFailure) return
  where errMsg = "please ensure CLIENT_ID & CLIENT_SECRET env vars are set"

gDriveInit :: SnapletInit b GoogleDrive
gDriveInit = makeSnaplet "google-drive" "google drive snaplet" Nothing $ do
  config <- loadConfig
  addRoutes [ ("/boo", sendToSlackHandler)
            , ("/sign-in", method GET $ signInHandler config)
            , ("/redirect-auth", method GET $ redirectHandler config)
            ]
  return GoogleDrive
