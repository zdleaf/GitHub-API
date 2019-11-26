module Lib
    ( callAPI
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as BS
--import Data.ByteString.Lazy     as BL
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Data.ByteString.Char8 as C8

import Network.HTTP.Simple -- see https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md
import Network.HTTP.Client.TLS
import Network.HTTP.Types

{-
wireshark filter: (ip.dst == 140.82.118.5)

case insensitive --- CI.mk $ 
-}

eventsAPIUrl = "http://api.github.com/events"
userAgentBS = C8.pack "https://github.com/zdleaf/GitHub-API"

--callAPI :: String -> IO BS.ByteString
callAPI :: String -> IO()
callAPI url = do
    initReq <- parseRequest $ url
    let request = setRequestHeaders [(hUserAgent, userAgentBS)] initReq
    response <- httpBS request
    return $ getResponseStatusCode response
    --return $ getResponseBody response -- JSON response data
    putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    --print $ getResponseHeader "Content-Type" response