module Main where

import HTTP as HT
import DB
import Parser

import Data.ByteString.Lazy as BL

main :: IO ()
main = do
    -- get repositories
    response <- callRepoAPI repoAPIUrl :: IO BL.ByteString
    db <- initialiseDB "github.db"
    print $ "length of response: " ++ (show $ BL.length response)
    --Prelude.writeFile ("output.json") (C8.unpack response)
    parsed <- parseResponse response
    addRepoMany db $ extractResp parsed
    contributorsUrllist <- retrieveContributorUrls db
    languageUrllist <- retrieveLanguageUrls db
    return ()
