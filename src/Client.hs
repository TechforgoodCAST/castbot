module Client where

import Control.Concurrent  (threadDelay)
import Control.Monad       (forever)
import Environment         (loadGDriveConfig)
import GoogleDrive         (authorizeInternalPoll)
import Network.HTTP.Simple

pollForNewFiles :: IO ()
pollForNewFiles = forever $ do
  threadDelay $ 60 * 1000000
  let r = "GET http://localhost:8000/google-drive/check-files"
  conf <- loadGDriveConfig
  req  <- authorizeInternalPoll conf <$> parseRequest r
  httpLBS req >>= print
