module HTTP
    ( callAPI,
    repoAPIUrl
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as BS
--import Data.ByteString.Lazy     as BL
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Data.ByteString.Char8 as C8

import Network.HTTP.Simple as HT -- see https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md
import Network.HTTP.Client.TLS
import Network.HTTP.Types

import DataTypes

{-
case insensitive --- CI.mk $ 

repoID: 224,238,000
-}

repoID = 224238000 :: Integer
--eventsAPIUrl = "http://api.github.com/events"
repoAPIUrl = "http://api.github.com/repositories?since=" ++ show repoID
userAgentBS = C8.pack "https://github.com/zdleaf/GitHub-API"

--callAPI returns JSON data from calling a GitHub API url 
callAPI :: String -> IO BS.ByteString
callAPI url = do
    initReq <- parseRequest $ url
    let request = setRequestHeaders [(hUserAgent, userAgentBS)] initReq
    response <- HT.httpBS request
    putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    return $ getResponseBody response -- JSON response data
    --print $ getResponseHeader "Content-Type" response
    --return $ getResponseStatusCode response

--getLanguagesURLS response = do