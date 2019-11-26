module HTTP
    ( callAPI,
    repoAPIUrl,
    parseResponse
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as BS
import Data.ByteString.Lazy     as BL
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Data.ByteString.Char8 as C8

import Network.HTTP.Simple -- see https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md
import Network.HTTP.Client.TLS
import Network.HTTP.Types

import DataTypes

-- JSON modules
import Data.Aeson as AE
--import Data.Aeson.TH

import Data.Text

{-
case insensitive --- CI.mk $ 

parsing many with Aeson:
https://geekingfrog.com/blog/post/struggles-with-parsing-json-with-aeson
https://artyom.me/aeson

repoID: 224,238,000
-}

repoID = 224238000 :: Integer
--eventsAPIUrl = "http://api.github.com/events"
repoAPIUrl = "http://api.github.com/repositories?since=" ++ show repoID
userAgentBS = C8.pack "https://github.com/zdleaf/GitHub-API"

--callAPI returns JSON data from calling a GitHub API url 
callAPI :: String -> IO BL.ByteString
callAPI url = do
    initReq <- parseRequest $ url
    let request = setRequestHeaders [(hUserAgent, userAgentBS)] initReq
    response <- httpLBS request
    Prelude.putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    return $ getResponseBody response -- JSON response data
    --print $ getResponseHeader "Content-Type" response
    --return $ getResponseStatusCode response

-- $(deriveJSON defaultOptions ''Reporesponse)

jsonFile :: FilePath
jsonFile = "output.json"

getJSON :: IO BL.ByteString
getJSON = BL.readFile jsonFile

parseResponse response = do
    --let decoded = AE.decode response :: Maybe Reporesponse
    --print decoded
    let d = eitherDecode response :: Either String Reporesponse
    case d of
        Left err -> Prelude.putStrLn err
        Right ps -> print ps

    