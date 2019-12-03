{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module DataTypes
(
   RepoResponse(id, languages_url, contributors_url, RepoResponse),
   ContributorResponse(login, ContributorResponse),
   LangResponse(repoID, language, lineCount)

) where

import Data.Aeson
import GHC.Generics

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

data ContributorResponse = ContributorResponse
  {
    login :: String
  }  deriving (Show, Generic)

instance FromJSON ContributorResponse

data LangResponse a = LangResponse
  {
    repoID :: Integer,
    language  :: String,
    lineCount :: Integer
  }  deriving (Show)
