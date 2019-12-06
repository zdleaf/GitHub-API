{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DataTypes
(
   RepoResponse(id, languages_url, contributors_url, RepoResponse),
   Contributor(login, repoID, contributors, ContributorFrom, ContributorTo),
   Language(langRepoID, language, lineCount, LanguageFrom, LanguageTo),
   TotalCount(TotalCount, totalLanguage, totalLineCount, totalContributors, linesPerContrib),
   AvgContribLines(repo, avgLinesPerContrib, AvgContribLines)
) where

import Data.Aeson
import GHC.Generics
import qualified Data.HashMap.Strict as H

-- |A data type to handle API response for public repositories
data RepoResponse = RepoResponse
    {
      id :: Integer, -- github repo id
      languages_url :: String, -- languages
      contributors_url :: String
    }
    deriving (Show, Generic)

-- | no need to specify details since we're deriving Generic
instance FromJSON RepoResponse
instance ToJSON RepoResponse

-- | A data type for contributors with multiple constructors reflecting different needs when encoding and decoding from JSON 
data Contributor = 
  ContributorFrom {
    login :: String
  } | ContributorTo {
    repoID :: Integer,
    contributors :: Integer
  } deriving (Show, Generic)

instance FromJSON Contributor
instance ToJSON Contributor where
  -- remove "tag" in JSON due to multiple constructors
  toJSON = genericToJSON (defaultOptions { sumEncoding = UntaggedValue }) 

-- | A data type for languages with multiple constructors reflecting different needs when encoding and decoding from JSON
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
  -- remove "tag" in JSON due to multiple constructors
  toJSON = genericToJSON (defaultOptions { sumEncoding = UntaggedValue }) 


-- | A data type for extracting our derived table of total counts 
data TotalCount = TotalCount
  {
    totalLanguage :: String,
    totalLineCount :: Integer,
    totalContributors :: Integer,
    linesPerContrib :: Double
  } deriving (Show, Generic)

instance ToJSON TotalCount


-- | A data type for extracting our derived table of lines per contributor
data AvgContribLines = AvgContribLines
  {
    repo :: String,
    avgLinesPerContrib :: Double
  } deriving (Show, Generic)

instance ToJSON AvgContribLines
