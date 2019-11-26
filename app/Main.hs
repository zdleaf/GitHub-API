module Main where

import Lib
import Githubdb
import HTTP

eventsAPIUrl = "https://api.github.com/events"

main :: IO ()
main = do
    response <- callAPI eventsAPIUrl
    print $ response
