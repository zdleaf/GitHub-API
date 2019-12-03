module HTTP
    ( callAPI,
    repoAPIUrl,
    callContribURL,
    callContribURLMany
    ) where

import Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import DataTypes as D
import Network.HTTP.Simple -- see https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Parser
import Data.Aeson.Types

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

--callRepoAPI returns JSON data from calling a GitHub API url
callAPI :: String -> IO BL.ByteString
callAPI url = do
    initReq <- parseRequest $ url
    let request = setRequestHeaders [(hUserAgent, userAgentBS),(hAuthorization, token)] initReq
    response <- httpLBS request
    Prelude.putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    return $ getResponseBody response -- JSON response data
    --print $ getResponseHeader "Content-Type" response
    --return $ getResponseStatusCode response

getContribCount (Right x) = x
getContribCount _ = 0

test = RepoResponse 1 "test.com" "https://api.github.com/repos/Chekist322/android-dagger/contributors"

callContribURL reporesponse = do
    response <- callAPI $ D.contributors_url reporesponse
    parsed <- parseContribResponse response
    let eitherCount = fmap Prelude.length parsed
    let count = getContribCount eitherCount
    print ((D.id reporesponse), count)
    return ((D.id reporesponse), count)

callContribURLMany [] = []
callContribURLMany (x:xs) = callContribURL x:callContribURLMany xs
