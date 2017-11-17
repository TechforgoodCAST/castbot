module Poll (pollForNewFiles) where

import Control.Concurrent  (threadDelay)
import Control.Exception
import Control.Monad       (forever)
import GoogleDrive
import Network.HTTP.Simple

pollForNewFiles :: IO ()
pollForNewFiles = poll `catch` printShutdown

printShutdown :: HttpException -> IO ()
printShutdown _ = putStrLn "Server shutdown: unreachable from internal request"

poll :: IO ()
poll = forever $ do
  threadDelay $ 30 * 1000000
  let r = "GET https://castmin-bot.herokuapp.com/google-drive/check-files"
  conf <- loadGDriveConfig
  req  <- authorizeInternalPoll conf <$> parseRequest r
  httpLBS req >>= print
