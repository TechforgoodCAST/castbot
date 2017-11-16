{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive where

import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson
import Data.ByteString.Char8     (pack)
import Data.Foldable             (fold)
import Data.Monoid               ((<>))
import Database                  (redisConnectInfo)
import GHC.Generics              (Generic)
import GoogleDrive.OAuth
import GoogleDrive.Types
import Network.HTTP.Simple
import Snap.Core                 (Method (..), method, writeLBS)
import Snap.Snaplet
import Snap.Snaplet.RedisDB
import System.Environment        (lookupEnv)
import System.Exit               (exitFailure)
import Util                      (printFail)

-- Config

getGDriveConfig :: MonadIO m => m (Maybe Config)
getGDriveConfig = liftIO . runMaybeT $
  Config <$> mtLookup "CLIENT_ID"
         <*> mtLookup "CLIENT_SECRET"
         <*> mtLookup "REDIRECT_URI"
  where mtLookup x = pack <$> (MaybeT $ lookupEnv x)

loadGDriveConfig :: MonadIO m => m Config
loadGDriveConfig =
  getGDriveConfig >>= maybe fail return
  where fail = liftIO $ printFail msg
        msg  = "please set CLIENT_ID, CLIENT_SECRET & REDIRECT_URI env vars"


-- Snaplet Init

gDriveInit :: SnapletInit b GoogleDrive
gDriveInit = makeSnaplet "google-drive" "google drive snaplet" Nothing $ do
  gDriveConfig <- loadGDriveConfig
  connInfo     <- liftIO redisConnectInfo
  redis        <- nestSnaplet "db" db $ redisDBInit connInfo
  addRoutes [ ("/sign-in",       get signInHandler)
            , ("/redirect-auth", get redirectHandler)
            , ("/auth-success",  get authSuccessHandler)
            ]
  return $ GoogleDrive redis gDriveConfig
  where get = method GET
