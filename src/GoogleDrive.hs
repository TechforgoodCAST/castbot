{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy   (ByteString)
import Data.Foldable          (fold)
import Data.Monoid            ((<>))
import Data.Text
import GHC.Generics           (Generic)
import GoogleDrive.OAuth
import GoogleDrive.Types
import Network.HTTP.Simple
import Snap.Core              (Method (..), method, writeLBS)
import Snap.Snaplet
import System.Environment

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

getConfig :: MonadIO m => m Config
getConfig = liftIO $ do
  cId <- getEnv "CLIENT_ID"
  cSc <- getEnv "CLIENT_SECRET"
  let redirectUri = "http://localhost:8000/google-drive/redirect-auth"
  return $ Config (pack cId) (pack cSc) redirectUri

gDriveInit :: SnapletInit b GoogleDrive
gDriveInit = makeSnaplet "google-drive" "google drive snaplet" Nothing $ do
  config <- getConfig
  addRoutes [ ("/boo", sendToSlackHandler)
            , ("/sign-in", method GET $ signInHandler config)
            , ("/redirect-auth", method GET $ redirectHandler config)
            ]
  return GoogleDrive
