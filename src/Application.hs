{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Application where

import Control.Lens        (makeLenses)
import Data.ByteString     (ByteString)
import GoogleDrive
import GoogleDrive.Types   (GoogleDrive)
import Snap.Core
import Snap.Http.Server
import Snap.Snaplet
import Snap.Util.FileServe
import System.Environment
import System.Exit         (exitFailure)

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

loadPort :: IO Int
loadPort = lookupEnv "PORT" >>= maybe fail (return . read)
  where
    fail   = putStrLn errMsg >> exitFailure
    errMsg = "please set the PORT env var"

app :: IO ()
app = do
  p <- loadPort
  let config = setPort p defaultConfig
  serveSnaplet config appInit
