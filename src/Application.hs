{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Application where

import Client              (pollForNewFiles)
import Control.Concurrent  (forkIO)
import Control.Lens        (makeLenses)
import Environment         (loadSnapServerPort)
import GoogleDrive
import Snap.Core
import Snap.Http.Server
import Snap.Snaplet
import Snap.Util.FileServe (serveDirectory)

newtype App = App
  { _googleDrive :: Snaplet GoogleDrive }

makeLenses ''App

appInit :: SnapletInit App App
appInit = makeSnaplet "castmin-bot" "castmin slack bot" Nothing $ do
  g <- nestSnaplet "google-drive" googleDrive gDriveInit
  addRoutes [ ("/", serveDirectory "static")
            , ("/", infoHandler)
            ]
  return $ App g

infoHandler :: Handler App App ()
infoHandler = writeText "Visit /google-drive/sign-in to link google drive with slack"

app :: IO ()
app = do
  p <- loadSnapServerPort
  let config = setPort p defaultConfig
  forkIO pollForNewFiles
  serveSnaplet config appInit
