module Client where

import Control.Concurrent  (threadDelay)
import Control.Monad       (when)
import Data.DateTime       (getCurrentTime, toGregorian)
import Data.Monoid         ((<>))
import Environment         (loadGDriveConfig, loadOrigin)
import GoogleDrive.Request (authorizeInternalPoll)
import Network.HTTP.Simple (httpLBS, parseRequest)
import System.IO

-- Internal HTTP Client that checks for new files periodically

pollForNewFiles :: IO ()
pollForNewFiles = do
  shouldPoll <- withinOfficeHours
  when shouldPoll $ do
    waitFor 10
    triggerCheck
    pollForNewFiles

triggerCheck :: IO ()
triggerCheck = do
  origin <- loadOrigin
  conf   <- loadGDriveConfig
  let rawRequest = "GET " <> origin <> "/google-drive/check-files"
  req    <- authorizeInternalPoll conf <$> parseRequest rawRequest
  httpLBS req >>= hPrint stderr

withinOfficeHours :: IO Bool
withinOfficeHours = do
  let morning = 8
      evening = 18
  (_, _, _, hr, _, _) <- toGregorian <$> getCurrentTime
  return $ hr >= morning && hr <= evening

waitFor :: Int -> IO ()
waitFor n = threadDelay $ n * 1000000
