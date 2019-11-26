module GithubDataTypes where
import Data.Aeson

-- a data type to handle api respones
data Reporesponse = Reporesponse
               {
                  id :: Integer,   -- github api id
                  languages_url :: String,
               }
               deriving (Show, Data, Generic)


instance fromJSON Reporesponse where
                     parseJSON = withObject "Reporesponse" $ \o -> do
                     id <- o.: "Id"
                     languages_url <-o.: "Languages_url"
                     return(Reproresponses id languages_url)
                  
                  
instance ToJSON Repropresponses





