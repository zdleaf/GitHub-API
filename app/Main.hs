module Main where
import System.IO

import HTTP as HT
import DB
import Parser
import DataTypes

import Data.ByteString.Lazy as BL

main :: IO ()
main = do
    -- get repositories
    print "retrieving repository information..."
    repoResponse <- callAPI repoAPIUrl :: IO BL.ByteString
    print $ "length of response: " ++ (show $ BL.length repoResponse)
    --Prelude.writeFile ("output.json") (C8.unpack response)
    print "parsing JSON..."
    repoParsed <- parseRepoResponse repoResponse

    print "initialising db..."
    db <- initialiseDB "github.db"

    print "adding repos to DB..."
    addRepoMany db $ extractResp repoParsed

    repoList <- retrieveRepoResponse db

    print "calling all contributor urls..."
    contribResp <- sequence $ fmap callContribURL repoList

    print "adding contributors to db..."
    sequence_ $ fmap (addContribs db) contribResp

    print "calling all language urls..."
    langResp <- sequence $ fmap callLangURL repoList

    print "adding languages to db..."
    sequence_ $ fmap (addLangMany db) langResp
    print "complete"


    return ()

