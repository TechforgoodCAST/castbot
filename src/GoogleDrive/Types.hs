{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive.Types where

import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.DateTime         (DateTime)
import Data.Text             (Text)
import Data.Text.Encoding    (decodeUtf8, encodeUtf8)

data Config =
  Config {
    clientId            :: ByteString
  , clientSecret        :: ByteString
  , redirectUri         :: ByteString
  , internalRouteSecret :: ByteString
  , webhookUrl          :: ByteString
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
newtype SlackPost = SlackPost Text deriving Show


-- Token Class (convenience for converting between ByteStrings)

class Token a where
  encodeToken :: a -> ByteString
  decodeToken :: ByteString -> a

instance Token AccessToken where
  encodeToken (AccessToken x) = encodeUtf8 x
  decodeToken                 = AccessToken . decodeUtf8

instance Token RefreshToken where
  encodeToken (RefreshToken x) = encodeUtf8 x
  decodeToken                  = RefreshToken . decodeUtf8

instance Token AuthCode where
  encodeToken (AuthCode x) = encodeUtf8 x
  decodeToken              = AuthCode . decodeUtf8


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

instance ToJSON SlackPost where
  toJSON (SlackPost x) =
    object [ "text" .= x
           , "attachments" .= [ object [ "fallback"  .= ("cat gif" :: Text)
                                       , "image_url" .= ("https://media.giphy.com/media/PUBxelwT57jsQ/giphy.gif" :: Text)
                                       ]
                              ]
           ]
