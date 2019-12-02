{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DataTypes
(
   Reporesponse(id, languages_url, contributorsURL, Reporesponse),
   Parameteresponse(gitid, rest, Parameteresponse)
) where

import Data.Aeson
import GHC.Generics

-- a data type to handle API response for public repos
data Reporesponse = Reporesponse
    {
      id :: Integer, -- github repo id
      languages_url :: String, -- languages
      contributorsURL :: String
    }
    deriving (Eq, Show)

instance FromJSON Reporesponse where
-- we do not need to specify details since we're deriving Generic
  parseJSON = withObject "Reporesponse" $ \o -> do
    id <- o .:  "id"
    languages_url  <- o .: "languages_url"
    contributorsURL  <- o .:? "contributorsURL" .!= "https://api.github.com/repos/"
    return Reporesponse{..}

data Parameteresponse a = Parameteresponse
  {
    gitid :: Integer,
    rest  :: a
  }  deriving (Show, Generic)