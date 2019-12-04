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
    repoResponse <- callAPI repoAPIUrl :: IO BL.ByteString
    db <- initialiseDB "github.db"
    print $ "length of response: " ++ (show $ BL.length repoResponse)
    --Prelude.writeFile ("output.json") (C8.unpack response)
    repoParsed <- parseRepoResponse repoResponse
    
    -- add repos to DB
    addRepoMany db $ extractResp repoParsed

    -- retrieve repos from DB (unnecessary here? just call on parsed)
    repoList <- retrieveRepoResponse db

    -- recusively call the contrib urls
    contrib <- sequence $ fmap callContribURL repoList
    print contrib
    -- add contrib counts per repo to DB
    addContribsMany db contrib
    --fmap (fmap (addContribs db)) contrib

    let lang = "https://api.github.com/repos/gr3gburk3/node/languages"
    langResp <- callAPI lang
    langParsed <- parseLangResponse langResp
    print langParsed

    return ()

