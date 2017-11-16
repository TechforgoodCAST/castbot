{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GoogleDrive.Types where

import Control.Lens         (makeLenses)
import Control.Monad.State  (get)
import Data.Aeson
import Data.Text            (Text)
import GHC.Generics         (Generic)
import Snap.Snaplet         (Snaplet)
import Snap.Snaplet.RedisDB (RedisDB)

data Config =
  Config {
    clientId     :: Text
  , clientSecret :: Text
  , redirectUri  :: Text
  }

data GoogleDrive =
  GoogleDrive {
    _db   :: Snaplet RedisDB
  , _conf :: Config
  }

makeLenses ''GoogleDrive

newtype AuthCode = AuthCode Text deriving Show

data AuthResponse =
  AuthResponse {
    accessToken  :: AccessToken
  , refreshToken :: Maybe RefreshToken
  } deriving (Show)

newtype AccessToken  = AccessToken Text  deriving (Show, Generic)
newtype RefreshToken = RefreshToken Text deriving (Show, Generic)

instance FromJSON AuthResponse where
  parseJSON (Object v) =
    AuthResponse <$> v .:  "access_token"
                 <*> v .:? "refresh_token"

instance FromJSON AccessToken
instance FromJSON RefreshToken
