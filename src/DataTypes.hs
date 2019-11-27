{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes
( Reporesponse
) where

import Data.Aeson
import GHC.Generics

-- a data type to handle API response for public repos
data Reporesponse = Reporesponse
               {
                  id :: Integer, -- github repo id
                  languages_url :: String, -- languages 
                  followers_url :: String 
               }
               deriving (Eq, Show, Generic)

instance FromJSON Reporesponse
-- we do not need to specify details since we're deriving Generic


