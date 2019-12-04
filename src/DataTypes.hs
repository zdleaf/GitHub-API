{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module DataTypes
(
   RepoResponse(id, languages_url, contributors_url, RepoResponse),
   Contributor(login, ContributorResponse),
   LangResponse(language, lineCount, LangResponse)

) where

import Data.Aeson
import GHC.Generics
import qualified Data.HashMap.Strict as H

-- a data type to handle API response for public repos
data RepoResponse = RepoResponse
    {
      id :: Integer, -- github repo id
      languages_url :: String, -- languages
      contributors_url :: String
    }
    deriving (Eq, Show, Generic)

instance FromJSON RepoResponse
-- we do not need to specify details since we're deriving Generic
instance ToJSON RepoResponse

data Contributor = ContributorFrom
  {
    login :: String
  } | ContributorTo {
    repoID :: Integer,
    contributors :: Integer
  } deriving (Show, Generic)

instance FromJSON Contributor

data LangResponse = LangResponse
  {
    language  :: String,
    lineCount :: Integer
  }  deriving (Show)

data Contributor = Contributor
  {
    repoID :: Integer,
    contributors :: Integer
  } deriving (Show, Generic)

instance ToJSON Contributor