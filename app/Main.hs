module Main where

import System.IO

import HTTP as HT
import DB
import Parser
import DataTypes
import Prelude as P
import Data.ByteString.Lazy as BL

import Data.Aeson.Encode.Pretty

{- startRepoId = 224238000 :: Integer
endRepoId = 224239000 :: Integer -}

main :: IO ()
main = do
    print "initialising db..."
    db <- initialiseDB "github.db"

    -- get the repository API responses for the repoIDs between the values in the 2nd and 3rd arguments
    -- callMultiRepo db StartRepoID EndRepoID
    getManyRepos db 224239200 224239300
{-     -- get repositories
    print "retrieving repository information..."
    repoResponse <- callAPI repoAPIUrl :: IO BL.ByteString
    print $ "length of response: " ++ (show $ BL.length repoResponse)
    --P.writeFile ("output.json") (C8.unpack response)
    print "parsing JSON..."
    repoParsed <- parseRepoResponse repoResponse
    print "adding repos to DB..."
    addRepoMany db $ extractResp repoParsed -}

    repoList <- retrieveDB db "repoResponses" repoFromSQL

    print "calling all contributor urls..."
    contribResp <- sequence $ fmap callContribURL repoList

    P.putStrLn "\nadding contributors to db..."
    sequence_ $ fmap (addContribs db) contribResp

    P.putStrLn "\ncalling all language urls..."
    langResp <- sequence $ fmap callLangURL repoList

    P.putStrLn "\nadding languages to db..."
    sequence_ $ fmap (addLangMany db) langResp
    fillTotalCount db

    P.putStrLn "\ndumping DB to JSON files..."
    repoJSONtoFile db
    contribJSONtoFile db
    langJSONtoFile db
    totalJSONtoFile db
    
    P.putStrLn "\ncomplete"

    return ()

