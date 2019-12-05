module Main where

import HTTP as HT
import DB
import Parser
import DataTypes

import Prelude as P
import System.IO

import Data.ByteString.Lazy as BL
import Data.Aeson.Encode.Pretty

main :: IO ()
main = do
    print "initialising db..."
    db <- initialiseDB "github.db"

    -- get the repository API responses for the repoIDs between the values in the 2nd and 3rd arguments
    -- getManyRepos db StartRepoID EndRepoID
    getManyRepos db 224240000 224241500

    repoList <- retrieveDB db "repoResponses" repoFromSQL

    print "calling all contributor urls..."
    contribResp <- sequence $ fmap callContribURL repoList

    P.putStrLn "\nadding contributors to db..."
    sequence_ $ fmap (addContribs db) contribResp

    P.putStrLn "\ncalling all language urls..."
    langResp <- sequence $ fmap callLangURL repoList

    P.putStrLn "\nadding languages to db..."
    sequence_ $ fmap (addLangList db) langResp
    fillTotalCount db
    fillLinesPerContrib db

    P.putStrLn "\ndumping DB to JSON files..."
    dbTableToJSON db "repoResponses" repoFromSQL
    dbTableToJSON db "contributorResponses" contribFromSQL
    dbTableToJSON db "langResponses" langFromSQL
    dbTableToJSON db "totalCount" totalFromSQL
    
    P.putStrLn "\ncomplete"

    return ()

