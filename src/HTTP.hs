module HTTP
    ( callAPI,
    repoAPIUrl
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

--callRepoAPI returns JSON data from calling a GitHub API url
callAPI :: String -> IO BL.ByteString
callAPI url = do
    initReq <- parseRequest $ url
    let request = setRequestHeaders [(hUserAgent, userAgentBS)] initReq
    response <- httpLBS request
    Prelude.putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    return $ getResponseBody response -- JSON response data
    --print $ getResponseHeader "Content-Type" response
    --return $ getResponseStatusCode response




