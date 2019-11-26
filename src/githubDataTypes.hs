module GithubDataTypes where


-- a data type to handle api respones
data Reporesponse = Reporesponse
               {
                  repoId :: Integer,   -- github api id
               }
               deriving (Eq, Show, Read)

