{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive.Types where

import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.DateTime         (DateTime)
import Data.Text             (Text)
import Data.Text.Encoding    (decodeUtf8, encodeUtf8)

data GDriveConfig =
  GDriveConfig {
    clientId         :: ByteString
  , clientSecret     :: ByteString
  , redirectUri      :: ByteString
  , pollingSecretKey :: ByteString
  , webhookUrl       :: ByteString
  , slackToken       :: ByteString
  }

newtype AuthCode     = AuthCode Text deriving Show
newtype AccessToken  = AccessToken Text deriving Show
newtype RefreshToken = RefreshToken Text deriving (Eq, Show)

data File =
  File {
    title       :: Text
  , link        :: Text
  , createdDate :: DateTime
  }

newtype Files = Files [File]
newtype Gif   = Gif Text

data SlackPost =
  SlackPost {
    files     :: Files
  , randomGif :: Gif
  }


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
    File <$> v .: "title"
         <*> v .: "alternateLink"
         <*> v .: "createdDate"

instance ToJSON SlackPost where
  toJSON (SlackPost (Files xs) gif) =
    object [ "text" .= ("Some new files have been added to the proposals folder!" :: Text)
           , "attachments" .= (map toJSON xs ++ [ toJSON gif ])
           ]

instance ToJSON File where
  toJSON (File title link _) =
    object [ "fallback" .= title
           , "pretext" .= title
           , "title" .= title
           , "title_link" .= link
           , "color" .= ("#03e07b" :: Text)
           ]

instance ToJSON Gif where
  toJSON (Gif gifUrl) =
    object [ "fallback"  .= ("random gif" :: Text)
           , "image_url" .= gifUrl
           ]
