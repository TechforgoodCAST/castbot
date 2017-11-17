{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive.Types where

import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.DateTime         (DateTime)
import Data.Text             (Text)
import Data.Text.Encoding    (encodeUtf8)

data Config =
  Config {
    clientId     :: ByteString
  , clientSecret :: ByteString
  , redirectUri  :: ByteString
  }

newtype AuthCode     = AuthCode Text deriving Show
newtype AccessToken  = AccessToken Text  deriving Show
newtype RefreshToken = RefreshToken Text deriving Show

data File =
  File {
    title         :: Text
  , link          :: Text
  , thumbnailLink :: Maybe Text
  , createdDate   :: DateTime
  } deriving Show

newtype Files = Files [File] deriving Show


-- Token Class (convenience for turning to ByteStrings)

class Token a where
  encodeToken :: a -> ByteString

instance Token AccessToken where
  encodeToken (AccessToken x) = encodeUtf8 x

instance Token RefreshToken where
  encodeToken (RefreshToken x) = encodeUtf8 x

instance Token AuthCode where
  encodeToken (AuthCode x) = encodeUtf8 x


-- JSON Instances

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
