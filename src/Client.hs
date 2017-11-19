module Client (pollForNewFiles) where

import Control.Concurrent  (threadDelay)
import Control.Exception   (catch)
import Control.Monad       (forever)
import Environment         (loadGDriveConfig)
import GoogleDrive         (authorizeInternalPoll)
import Network.HTTP.Simple

pollForNewFiles :: IO ()
pollForNewFiles = poll `catch` printShutdown

printShutdown :: HttpException -> IO ()
printShutdown _ = putStrLn "Server shutdown: unreachable from internal request"

poll :: IO ()
poll = forever $ do
  threadDelay $ 10 * 1000000
  let r = "GET http://localhost:8000/google-drive/check-files"
  conf <- loadGDriveConfig
  req  <- authorizeInternalPoll conf <$> parseRequest r
  httpLBS req >>= print
