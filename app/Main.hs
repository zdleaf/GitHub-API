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

    let startRepoID = 224244000
    let endRepoID   = 224244100

    getManyRepos db startRepoID endRepoID

    repoList <- retrieveDB db "repoResponses" repoFromSQL

    print "calling all contributor urls..."
    contribResp <- sequence $ fmap callContribURL repoList

    P.putStrLn "\nadding contributors to db..."
    sequence_ $ fmap (addContribs db) contribResp

    P.putStrLn "\ncalling all language urls..."
    langResp <- sequence $ fmap callLangURL repoList

    P.putStrLn "\nadding languages to db..."
    sequence_ $ fmap (addLangList db) langResp

    P.putStrLn "\nderiving and inserting values for totalCount and linesPerContrib db tables..."
    fillTotalCount db
    fillLinesPerContrib db

    P.putStrLn "\ndumping DB to JSON files..."
    dbTableToJSON db "repoResponses" repoFromSQL
    dbTableToJSON db "contributorResponses" contribFromSQL
    dbTableToJSON db "langResponses" langFromSQL
    dbTableToJSON db "totalCount" totalFromSQL
    dbTableToJSON db "linesPerContrib" avgContribFromSQL

    P.putStrLn "\nResults:"
    topFiveLangs db
    topFiveContribs db
    topLinesPerContrib db
    return ()

