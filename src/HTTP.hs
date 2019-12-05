module HTTP
    ( callAPI,
    callContribURL,
    callLangURL,
    getManyRepos
    ) where

import Parser
import DataTypes as D
import DB

import Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8

import Network.HTTP.Simple -- see https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md
import Network.HTTP.Client.TLS
import Network.HTTP.Types

import Data.Aeson.Types
import System.IO

repoAPIBase = "http://api.github.com/repositories?since="
userAgentBS = C8.pack "https://github.com/zdleaf/GitHub-API"
token =  C8.pack "token da45c3b3cfa3127bf08e60eff8be3f58aac0923d"

--callRepoAPI returns JSON data from calling a GitHub API url
callAPI :: String -> IO BL.ByteString
callAPI url = do
    initReq <- parseRequest $ url
    let request = setRequestHeaders [(hUserAgent, userAgentBS),(hAuthorization, token)] initReq
    response <- httpLBS request
    --Prelude.putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    return $ getResponseBody response -- JSON response data
    --print $ getResponseHeader "Content-Type" response
    --return $ getResponseStatusCode response

getManyRepos db currentRepoID endRepoID = do
    print $ "getting repos from " ++ (show currentRepoID) ++ " to " ++ (show $ currentRepoID + 100)
    repoResponse <- callAPI $ repoAPIBase ++ (show currentRepoID)
    print $ "length of response: " ++ (show $ BL.length repoResponse)
    print "parsing JSON..."
    repoParsed <- parseRepoResponse repoResponse
    print "adding repos to DB..."
    addRepoMany db $ extractResp repoParsed
    -- run again until we reach the endRepoId
    let nextVal = currentRepoID + 100
    if nextVal <= endRepoID 
        then getManyRepos db nextVal endRepoID
        else print "completed calling all requested repos"
    
removeEitherNum (Right x) = x
removeEitherNum _ = 0

test = RepoResponse 999 "https://api.github.com/repos/gr3gburk3/node/languages" "https://api.github.com/repos/Chekist322/android-dagger/contributors"

callContribURL reporesponse = do
    response <- callAPI $ D.contributors_url reporesponse
    parsedContribs <- parseContribResponse response
    let eitherCount = fmap Prelude.length parsedContribs
    let count = removeEitherNum eitherCount
    Prelude.putStr "."
    hFlush stdout
    --print "((D.id reporesponse), count)"
    return ((D.id reporesponse), count)

{- removeEitherLang (Right x) = x
removeEitherLang _ = "error" -}

callLangURL reporesponse = do
    response <- callAPI $ D.languages_url reporesponse
    parsedLangs <- parseLangResponse response
    Prelude.putStr "."
    hFlush stdout
    return $ splitLangResp (D.id reporesponse) parsedLangs

splitLangResp id (Right []) = []
splitLangResp id (Right (x:xs)) = (id, D.language x, D.lineCount x):splitLangResp id (Right xs)
splitLangResp id _ = []