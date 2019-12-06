module HTTP
    ( callAPI,
    callContribURL,
    callLangURL,
    getManyRepos
    ) where

import Parser
import DataTypes as D
import DB

import System.IO
import Control.Exception (try)
import Control.Monad (when)

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.Aeson.Types
import Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8

import Network.HTTP.Simple -- see https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md
import Network.HTTP.Client.TLS
import Network.HTTP.Types

repoAPIBase = "http://api.github.com/repositories?since="
userAgentBS = C8.pack "https://github.com/zdleaf/GitHub-API"
token =  C8.pack "token 5be20b79c05ba422ca80377c1b759e9f99d5d335"
-- alternative token da45c3b3cfa3127bf08e60eff8be3f58aac0923d

-- | callAPI returns JSON data as a Lazy ByteString from calling a GitHub API URL
callAPI :: String -> IO BL.ByteString
callAPI url = do
    initReq <- parseRequest $ url
    let request = setRequestHeaders [(hUserAgent, userAgentBS),(hAuthorization, token)] initReq
    response <- try $ httpLBS request
    case response of
        Left e -> print (e :: HttpException)
        Right response -> return ()
    return $ getResponseBody (handleAPIException response)

handleAPIException (Left err) = undefined
handleAPIException (Right response) = response

-- | As the API only returns 100 repositories at once, getManyRepos recursively calls callAPI for multiple blocks of 100 repositories.
-- The function takes a database to write to, and a start repository ID and an end repository ID.
-- The API is called via the URL http://api.github.com/repositories?since= where we append a repository ID to receive the 100 repositories since that ID.
getManyRepos:: IConnection conn => conn -> Integer -> Integer -> IO ()
getManyRepos db currentRepoID endRepoID = do
    print $ "getting repos from " ++ (show currentRepoID) ++ " to " ++ (show $ currentRepoID + 99)
    repoResponse <- callAPI $ repoAPIBase ++ (show currentRepoID)
    print $ "length of response: " ++ (show $ BL.length repoResponse)
    print "parsing JSON..."
    repoParsed <- parseRepoResponse repoResponse
    print "adding repos to DB..."
    addRepoMany db $ extractResp repoParsed
    -- run again until we reach the endRepoId
    let nextVal = currentRepoID + 100
    if nextVal < endRepoID
        then getManyRepos db nextVal endRepoID
        else print "completed calling all requested repos"

-- | Error handling for callContribURL as parseContribResponse returns Either
removeEitherNum :: Num p => Either a p -> p
removeEitherNum (Right x) = x
removeEitherNum _ = 0

-- | takes a RepoResponse and calls the API on the Contributor URL and returns a tuple of repoID and contributor count
callContribURL :: RepoResponse -> IO (Integer, Int)
callContribURL reporesponse = do
    response <- callAPI $ D.contributors_url reporesponse
    parsedContribs <- parseContribResponse response
    let eitherCount = fmap Prelude.length parsedContribs
    let count = removeEitherNum eitherCount
    Prelude.putStr "."
    hFlush stdout
    --print "((D.id reporesponse), count)"
    return ((D.id reporesponse), count)

-- | takes a RepoResponse and calls the API on the languages URL and returns a list of tuples of repoID, language and line count
callLangURL :: RepoResponse -> IO [(Integer, String, Integer)]
callLangURL reporesponse = do
    response <- callAPI $ D.languages_url reporesponse
    parsedLangs <- parseLangResponse response
    Prelude.putStr "."
    hFlush stdout
    return $ splitLangResp (D.id reporesponse) parsedLangs


-- | Error handling for callLangURL when parseLangResponse is called as it returns an Either[]
splitLangResp :: t -> Either a [Language] -> [(t, String, Integer)]
splitLangResp id (Right []) = []
splitLangResp id (Right (x:xs)) = (id, D.language x, D.lineCount x):splitLangResp id (Right xs)
splitLangResp id _ = []