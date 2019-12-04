module Main where
import System.IO

import HTTP as HT
import DB
import Parser
import DataTypes
import Prelude as P
import Data.ByteString.Lazy as BL

main :: IO ()
main = do
    -- get repositories
    P.putStrLn "retrieving repository information..."
    repoResponse <- callAPI repoAPIUrl :: IO BL.ByteString
    P.putStrLn $ "length of response: " ++ (show $ BL.length repoResponse)
    --P.writeFile ("output.json") (C8.unpack response)
    P.putStrLn "parsing JSON..."
    repoParsed <- parseRepoResponse repoResponse

    P.putStrLn "initialising db..."
    db <- initialiseDB "github.db"

    P.putStrLn "adding repos to DB..."
    addRepoMany db $ extractResp repoParsed

    repoList <- retrieveRepoResponse db

    P.putStrLn "calling all contributor urls..."
    contribResp <- sequence $ fmap callContribURL repoList

    P.putStrLn "\nadding contributors to db..."
    sequence_ $ fmap (addContribs db) contribResp

    P.putStrLn "\ncalling all language urls..."
    langResp <- sequence $ fmap callLangURL repoList

    P.putStrLn "\nadding languages to db..."
    sequence_ $ fmap (addLangMany db) langResp
    fillTotalCount db

    P.putStrLn "\ncomplete"


    return ()

