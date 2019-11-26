module Main where

import HTTP as HT
import DB

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
    response <- callAPI repoAPIUrl
    db <- initialiseDB "github.db"
    print $ "length of response: " ++ (show $ BS.length response)
    writeFile ("output.json") (C8.unpack response)
    parseResponse response
    return ()