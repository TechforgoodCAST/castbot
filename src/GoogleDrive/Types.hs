{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GoogleDrive.Types where

import Control.Lens          (makeLenses)
import Control.Monad.State   (get)
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.DateTime
import Data.Text             (Text)
import Data.Text.Encoding    (encodeUtf8)
import GHC.Generics          (Generic)
import Snap.Snaplet          (Snaplet)
import Snap.Snaplet.RedisDB  (RedisDB)

data Config =
  Config {
    clientId     :: ByteString
  , clientSecret :: ByteString
  , redirectUri  :: ByteString
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

data File =
  File {
    title         :: Text
  , link          :: Text
  , thumbnailLink :: Maybe Text
  , createdDate   :: DateTime
  } deriving (Generic, Show)

newtype Files = Files [File] deriving Show

class Token a where
  encodeToken :: a -> ByteString

instance Token AccessToken where
  encodeToken (AccessToken x) = encodeUtf8 x

instance Token RefreshToken where
  encodeToken (RefreshToken x) = encodeUtf8 x

instance Token AuthCode where
  encodeToken (AuthCode x) = encodeUtf8 x

instance FromJSON AuthResponse where
  parseJSON (Object v) =
    AuthResponse <$> v .:  "access_token"
                 <*> v .:? "refresh_token"

instance FromJSON AccessToken where
  parseJSON (Object v) = AccessToken <$> v .: "access_token"

instance FromJSON RefreshToken where
  parseJSON (Object v) = RefreshToken <$> v .: "refresh_token"

instance FromJSON Files where
  parseJSON (Object v) = Files <$> v .: "items"

instance FromJSON File where
  parseJSON (Object v) =
    File <$> v .:  "title"
         <*> v .:  "alternateLink"
         <*> v .:? "thumbnailLink"
         <*> v .:  "createdDate"
