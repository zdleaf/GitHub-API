{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module DataTypes
(
   RepoResponse(id, languages_url, contributors_url, RepoResponse),
   Contributor(login, repoID, contributors, ContributorFrom, ContributorTo),
   Language(langRepoID, language, lineCount, LanguageFrom, LanguageTo),
   TotalCount(TotalCount, totalLanguage, totalLineCount, totalContributors)

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
    deriving (Show, Generic)

-- we do not need to specify details since we're deriving Generic
instance FromJSON RepoResponse
instance ToJSON RepoResponse


data Contributor = 
  ContributorFrom {
    login :: String
  } | ContributorTo {
    repoID :: Integer,
    contributors :: Integer
  } deriving (Show, Generic)

instance FromJSON Contributor
instance ToJSON Contributor where
  toJSON = genericToJSON (defaultOptions { sumEncoding = UntaggedValue }) --remove "tag" in JSON due to multiple constructors

data Language = 
  LanguageFrom
  {
    language  :: String,
    lineCount :: Integer
  } | LanguageTo {
    langRepoID :: Integer,
    language :: String,
    lineCount :: Integer
  } deriving (Show, Generic)

instance FromJSON Language
instance ToJSON Language where
  toJSON = genericToJSON (defaultOptions { sumEncoding = UntaggedValue }) --remove "tag" in JSON due to multiple constructors

data TotalCount = TotalCount
  {
    totalLanguage :: String, 
    totalLineCount :: Integer,
    totalContributors :: Integer
  } deriving (Show, Generic)

instance ToJSON TotalCount