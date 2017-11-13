{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive.Types where

import Data.Aeson
import Data.Text    (Text)
import Data.Word
import GHC.Generics (Generic)

data Config =
  Config {
    clientId     :: Text
  , clientSecret :: Text
  , redirectUri  :: Text
  }

newtype AuthCode = AuthCode Text deriving Show

data AuthResponse =
  AuthResponse {
    accessToken  :: Text
  , refreshToken :: Text
  } deriving (Show)

instance FromJSON AuthResponse where
  parseJSON (Object v) =
    AuthResponse <$> v .: "access_token"
                 <*> v .: "refresh_token"


newtype SlackPost = SlackPost { text :: Text } deriving (Generic)

data GoogleDrive = GoogleDrive

instance ToJSON SlackPost
