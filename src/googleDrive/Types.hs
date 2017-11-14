{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GoogleDrive.Types where

import Control.Lens                       (makeLenses)
import Control.Monad.State                (get)
import Data.Aeson
import Data.Text                          (Text)
import Data.Word
import Database.PostgreSQL.Simple.ToField (toField)
import GHC.Generics                       (Generic)
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

data Config =
  Config {
    clientId     :: Text
  , clientSecret :: Text
  , redirectUri  :: Text
  }

newtype GoogleDrive = GoogleDrive { _db :: Snaplet Postgres }

makeLenses ''GoogleDrive

instance HasPostgres (Handler b GoogleDrive) where
  getPostgresState = with db get

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

instance ToRow AccessToken
instance ToRow RefreshToken

instance FromRow AccessToken
instance FromRow RefreshToken
