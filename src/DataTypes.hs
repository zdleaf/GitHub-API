{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module DataTypes
(
   Reporesponse(id, languages_url, contributors_url, Reporesponse),
   Parameteresponse(gitid, rest, Parameteresponse)
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

instance FromJSON Reporesponse
-- we do not need to specify details since we're deriving Generic

data ContributorResponse = ContributorResponse
  {
    id :: Integer,
    contributors :: Integer
  }  deriving (Show, Generic)