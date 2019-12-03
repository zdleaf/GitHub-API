module Main where
import System.IO

import HTTP as HT
import DB
import Parser

import Data.ByteString.Lazy as BL

main :: IO ()
main = do
    -- get repositories
    response <- callAPI repoAPIUrl :: IO BL.ByteString
    db <- initialiseDB "github.db"
    print $ "length of response: " ++ (show $ BL.length response)
    --Prelude.writeFile ("output.json") (C8.unpack response)
    parsed <- parseRepoResponse response
    addRepoMany db $ extractResp parsed
    repoReponse <- retrieveRepoResponse db
    --print (repoReponse)
    hFlush stdout


    -- let contributors_url_test = "https://api.github.com/repos/edwinf/vscode-jest/stats/contributors"
    -- contributorsbytestring <- callAPI contributors_url_test
    -- test_parse <- parseSingleRespones contributorsbytestring
    -- print $ test_parse

    return ()
