module Client where

import Control.Concurrent  (threadDelay)
import Control.Monad       (forever)
import Data.Monoid         ((<>))
import Environment         (loadGDriveConfig, loadOrigin)
import GoogleDrive         (authorizeInternalPoll)
import Network.HTTP.Simple

pollForNewFiles :: IO ()
pollForNewFiles = forever $ do
  threadDelay $ 60 * 1000000
  origin <- loadOrigin
  let r = "GET " <> origin <> "/google-drive/check-files"
  conf <- loadGDriveConfig
  req  <- authorizeInternalPoll conf <$> parseRequest r
  httpLBS req >>= hPrint stderr
