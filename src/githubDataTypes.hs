module GithubDataTypes where


-- a data type to handle api respones
data Reporesponse = Reporesponse
               {
                  repoId :: Integer,   -- github api id
               }
               deriving (Eq, Show, Read)


-- A  data type to handle Languages api responses

data Langauges = Languages

      {
            languageURL :: String,

      }
      deriving(Show,Data, Typable)