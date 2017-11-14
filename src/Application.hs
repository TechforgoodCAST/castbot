{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Application where

import Control.Lens
import Data.ByteString    (ByteString)
import GoogleDrive
import GoogleDrive.Types
import Snap.Core
import Snap.Http.Server
import Snap.Snaplet
import System.Environment

newtype App = App
  { _googleDrive :: Snaplet GoogleDrive }

makeLenses ''App

appInit :: SnapletInit App App
appInit = makeSnaplet "castmin-bot" "castmin slack bot" Nothing $ do
  g <- nestSnaplet "google-drive" googleDrive gDriveInit
  addRoutes [("/", infoHandler)]
  return $ App g

infoHandler :: Handler App App ()
infoHandler = writeText "Visit /google-drive/sign-in to link google drive with slack"

app :: IO ()
app = do
  p <- read <$> getEnv "PORT"
  let conf = setPort p defaultConfig
  serveSnaplet conf appInit
