module Main where

import HTTP
import DB

import qualified Data.ByteString as BS

eventsAPIUrl = "https://api.github.com/events"

main :: IO ()
main = do
    response <- callAPI eventsAPIUrl
    db <- initialiseDB "github.db"
    print $ "length of response: " ++ (show $ BS.length response)
