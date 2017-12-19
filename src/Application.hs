{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Application where

import Client                   (pollForNewFiles)
import Control.Concurrent.Async
import Control.Lens             (makeLenses)
import Environment              (loadSnapServerPort)
import GoogleDrive.Snaplet
import Snap.Core
import Snap.Http.Server
import Snap.Snaplet
import Snap.Util.FileServe      (serveDirectory)

newtype App = App
  { _googleDrive :: Snaplet GoogleDrive }

makeLenses ''App


-- Top level Snap Application

appInit :: SnapletInit App App
appInit = makeSnaplet "castmin-bot" "castmin slack bot" Nothing $ do
  g <- nestSnaplet "google-drive" googleDrive gDriveInit
  addRoutes [ ("/", serveDirectory "static")
            , ("/", infoHandler)
            ]
  return $ App g

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
