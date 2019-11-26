module Main where

import Lib

eventsAPIUrl = "https://api.github.com/events"

main :: IO ()
main = do
    response <- callAPI eventsAPIUrl
    print $ response
