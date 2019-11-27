module Main where

import HTTP as HT
import DB
<<<<<<< HEAD
import Data.Aeson
import Data.Text
=======
import Parser
>>>>>>> 1536fda4dd1a8abbfa8ba6f4048a92f2968c6eb8

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy as BL

main :: IO ()
main = do
    response <- callAPI repoAPIUrl :: IO BL.ByteString
    db <- initialiseDB "github.db"
    print $ "length of response: " ++ (show $ BL.length response)
    --Prelude.writeFile ("output.json") (C8.unpack response)
    parseResponse response
    return ()