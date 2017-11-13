{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Application where

import Control.Lens
import Data.ByteString    (ByteString)
import GoogleDrive
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
  addRoutes routes
  return $ App g

routes :: [(ByteString, Handler App App ())]
routes =
  [ ("/", writeText "hello world")
  , ("/status", writeText "status OK")
  ]

app :: IO ()
app = do
  p <- read <$> getEnv "PORT"
  let conf = setPort p defaultConfig
  serveSnaplet conf appInit
