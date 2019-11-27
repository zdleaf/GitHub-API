module HTTP
    ( callAPI,
    repoAPIUrl,
    parseResponse
    ) where

import DataTypes

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

-- JSON modules
import Data.Aeson as AE
import Data.Aeson.Types
--import Data.Aeson.TH

import Data.Text
import Data.Foldable
import Control.Monad (join)

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

parseResponse response = do
    print $ eitherDecode response >>= parseEither verboseParseMany

{- change/rewrite the below functions - came from https://geekingfrog.com/blog/post/struggles-with-parsing-json-with-aeson -}
verboseParser :: Value -> Parser (Either String Reporesponse)
verboseParser v = do
    case parseEither parseJSON v of
        Left err -> return . Left $ err ++ " -- Invalid object is: " ++ show v
        Right parsed -> return $ Right parsed

verboseParseMany :: Value -> Parser [Either String Reporesponse]
verboseParseMany = withArray "Reporesponse" $ \arr -> do
    let allParsed = fmap (join . parseEither verboseParser) arr
    return $ toList allParsed
    