module Poll where

import Data.ByteString.Lazy
import GoogleDrive
import Network.HTTP.Simple

checkFiles :: IO (Response ByteString)
checkFiles = do
  conf <- loadGDriveConfig
  req  <- authorizeInternal conf <$> parseRequest "GET http://localhost:8000/google-drive/check-files"
  httpLBS req
