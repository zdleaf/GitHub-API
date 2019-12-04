module Main where
import System.IO

import HTTP as HT
import DB
import Parser
import DataTypes
import Prelude as P
import Data.ByteString.Lazy as BL

import Data.Aeson.Encode.Pretty

main :: IO ()
main = do
{-     -- get repositories
    print "retrieving repository information..."
    repoResponse <- callAPI repoAPIUrl :: IO BL.ByteString
    print $ "length of response: " ++ (show $ BL.length repoResponse)
    --P.writeFile ("output.json") (C8.unpack response)
    print "parsing JSON..."
    repoParsed <- parseRepoResponse repoResponse -}

    print "initialising db..."
    db <- initialiseDB "github.db"

{-     print "adding repos to DB..."
    addRepoMany db $ extractResp repoParsed -}

    repoJSONtoFile db
    contribJSONtoFile db
    langJSONtoFile db
    {- repoList <- retrieveDB db "repoResponses" repoFromSQL

    print "calling all contributor urls..."
    contribResp <- sequence $ fmap callContribURL repoList

    P.putStrLn "\nadding contributors to db..."
    sequence_ $ fmap (addContribs db) contribResp

    P.putStrLn "\ncalling all language urls..."
    langResp <- sequence $ fmap callLangURL repoList

    P.putStrLn "\nadding languages to db..."
    sequence_ $ fmap (addLangMany db) langResp
    fillTotalCount db

    P.putStrLn "\ncomplete"
 -}
    return ()

