module GithubDataTypes where


-- a data type to handle api respones
data Reporesponse = Reporesponse
               {
                  id :: Integer,   -- github api id
                  languages_url :: String,
               }
               deriving (Eq, Show, Data, Typable)


