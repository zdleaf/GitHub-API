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
    response <- callAPI repoAPIUrl :: IO BL.ByteString
    db <- initialiseDB "github.db"
    print $ "length of response: " ++ (show $ BL.length response)
    --Prelude.writeFile ("output.json") (C8.unpack response)
    parsed <- parseRepoResponse response

    -- add repos to DB
    addRepoMany db $ extractResp parsed

    -- retrieve repos from DB (unnecessary here? just call on parsed)
    repoResponse <- retrieveRepoResponse db

    -- recusively call the contrib urls
    -- contrib <- sequence $ fmap callContribURL repoResponse
    -- print contrib
    -- -- add contrib counts per repo to DB
    -- addContribsMany db contrib
    --fmap (fmap (addContribs db)) contrib

    let lang = "https://api.github.com/repos/gr3gburk3/node/languages"
    langResp <- callAPI lang
    langParsed <- parseLangResponse langResp
    print langParsed

    return ()

