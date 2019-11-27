{-# LANGUAGE DeriveGeneric #-}
module DataTypes
( Reporesponse
) where

import Data.Aeson
import GHC.Generics

instance FromJSON Reporesponse where
   parseJSON = genericParseJSON defaultOptions
      { omitNothingFields = True }
      
instance ToJSON Reporesponse

-- a data type to handle api respones
data Reporesponse = Reporesponse
               {
                  id :: Integer, -- github api id
                  languages_url :: String
               }
               deriving (Eq, Show, Generic)



