module Main where

import Lib
import Githubdb
import HTTP

import qualified Data.ByteString as BS

eventsAPIUrl = "https://api.github.com/events"

main :: IO ()
main = do
    response <- callAPI eventsAPIUrl
    print $ "length of response: " ++ (show $ BS.length response)
