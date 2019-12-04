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
    Prelude.putStrLn "retrieving repository information..."
    repoResponse <- callAPI repoAPIUrl :: IO BL.ByteString
    Prelude.putStrLn $ "length of response: " ++ (show $ BL.length repoResponse)
    --Prelude.writeFile ("output.json") (C8.unpack response)
    Prelude.putStrLn "parsing JSON..."
    repoParsed <- parseRepoResponse repoResponse

    Prelude.putStrLn "initialising db..."
    db <- initialiseDB "github.db"

    Prelude.putStrLn "adding repos to DB..."
    addRepoMany db $ extractResp repoParsed

    repoList <- retrieveRepoResponse db

    Prelude.putStrLn "calling all contributor urls..."
    contribResp <- sequence $ fmap callContribURL repoList

    Prelude.putStrLn "\nadding contributors to db..."
    sequence_ $ fmap (addContribs db) contribResp

    Prelude.putStrLn "\ncalling all language urls..."
    langResp <- sequence $ fmap callLangURL repoList

    Prelude.putStrLn "\nadding languages to db..."
    sequence_ $ fmap (addLangMany db) langResp
    Prelude.putStrLn "\ncomplete"


    return ()

