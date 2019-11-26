{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes
( Reporesponse
) where

import Data.Aeson
import GHC.Generics
import Control.Monad (mzero)

-- a data type to handle API response for public repos
data Reporesponse = Reporesponse
               {
                  id :: Integer, -- github api id
                  languages_url :: String
               }
               deriving (Eq, Show, Generic)

instance FromJSON Reporesponse
   {-
   parseJSON (Object v) =
      Reporesponse   <$> v .: "id"
                     <*> v .: "languages_url"
   parseJSON _ = mzero
   -}

