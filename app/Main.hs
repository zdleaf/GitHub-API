module Main where

import HTTP

eventsAPIUrl = "https://api.github.com/events"

main :: IO ()
main = do
    response <- callAPI eventsAPIUrl
    print $ response
