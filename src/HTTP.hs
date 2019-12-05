module HTTP
    ( callAPI,
    repoAPIUrl,
    callContribURL,
    callLangURL
    ) where

import Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import DataTypes as D
import Network.HTTP.Simple -- see https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Parser
import Data.Aeson.Types
import System.IO

{-
parsing many with Aeson:
https://geekingfrog.com/blog/post/struggles-with-parsing-json-with-aeson
https://artyom.me/aeson

repoID: 224,238,000
-}

startRepoId = 224238000 :: Integer
repoAPIUrl = "http://api.github.com/repositories?since=" ++ show startRepoId
userAgentBS = C8.pack "https://github.com/zdleaf/GitHub-API"
token =  C8.pack "token da45c3b3cfa3127bf08e60eff8be3f58aac0923d"

-- | callAPI returns JSON data from calling a GitHub API url
callAPI :: String -> IO BL.ByteString
callAPI url = do
    initReq <- parseRequest $ url
    let request = setRequestHeaders [(hUserAgent, userAgentBS),(hAuthorization, token)] initReq
    response <- httpLBS request
    --Prelude.putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    return $ getResponseBody response -- JSON response data
    --print $ getResponseHeader "Content-Type" response
    --return $ getResponseStatusCode response

-- | Error handling for callContribURL as parseContribResponse returns Either
removeEitherNum :: Num p => Either a p -> p
removeEitherNum (Right x) = x
removeEitherNum _ = 0

test = RepoResponse 999 "https://api.github.com/repos/gr3gburk3/node/languages" "https://api.github.com/repos/Chekist322/android-dagger/contributors"


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