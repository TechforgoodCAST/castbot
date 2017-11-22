module Client where

import Control.Concurrent  (threadDelay)
import Control.Monad       (when)
import Data.DateTime       (getCurrentTime, toGregorian)
import Data.Monoid         ((<>))
import Environment         (loadGDriveConfig, loadOrigin)
import GoogleDrive         (authorizeInternalPoll)
import Network.HTTP.Simple (httpLBS, parseRequest)

pollForNewFiles :: IO ()
pollForNewFiles = do
  shouldPoll <- isDuringTimeWindow
  when shouldPoll $ do
    threadDelay $ 10 * 1000000
    origin <- loadOrigin
    let r = "GET " <> origin <> "/google-drive/check-files"
    conf <- loadGDriveConfig
    req  <- authorizeInternalPoll conf <$> parseRequest r
    httpLBS req >>= print
    pollForNewFiles

isDuringTimeWindow :: IO Bool
isDuringTimeWindow = do
  (_, _, _, hr, _, _) <- toGregorian <$> getCurrentTime
  return $ hr >= 8 && hr <= 18
