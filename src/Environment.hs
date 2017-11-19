module Environment
  ( loadSnapServerPort
  , loadGDriveConfig
  , loadRedisConnectInfo
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString.Char8
import Database
import Database.Redis
import GoogleDrive.Types
import System.Environment
import System.Exit

-- Server Config

loadSnapServerPort :: IO Int
loadSnapServerPort = lookupEnv "PORT" >>= maybe fail (return . read)
  where fail = printFail "please set the PORT env var"


-- Google Drive Config

loadGDriveConfig :: MonadIO m => m GDriveConfig
loadGDriveConfig =
  getGDriveConfig >>= maybe fail return
  where fail = liftIO $ printFail msg
        msg  = mconcat [ "please set "
                       , "GDRIVE_CLIENT_ID, "
                       , "GDRIVE_CLIENT_SECRET, "
                       , "REDIRECT_URI, "
                       , "POLLING_SECRET_KEY, "
                       , "SLACK_WEBHOOK_URL, "
                       , "SLACK_VERIFICATION_TOKEN "
                       , "env vars"
                       ]

getGDriveConfig :: MonadIO m => m (Maybe GDriveConfig)
getGDriveConfig = liftIO . runMaybeT $
  GDriveConfig <$> mtLookup "GDRIVE_CLIENT_ID"
               <*> mtLookup "GDRIVE_CLIENT_SECRET"
               <*> mtLookup "REDIRECT_URI"
               <*> mtLookup "POLLING_SECRET_KEY"
               <*> mtLookup "SLACK_WEBHOOK_URL"
               <*> mtLookup "SLACK_VERIFICATION_TOKEN"
  where mtLookup x = pack <$> (MaybeT $ lookupEnv x)


-- Redis Connect Info

loadRedisConnectInfo :: IO ConnectInfo
loadRedisConnectInfo = parseConnection <$> getEnv "REDIS_URL" >>= either printFail return


-- Utils

printFail :: Show a => a -> IO b
printFail msg = print msg >> exitFailure
