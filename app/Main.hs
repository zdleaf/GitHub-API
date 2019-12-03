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
    addRepoMany db $ extractResp parsed
    repoReponse <- retrieveRepoResponse db

    {-     contrib <- callContribURL test -}
    
    contrib <- sequence $ fmap callContribURL test
    addContribsMany db contrib

{-     let lang = "https://api.github.com/repos/liuzi919/biocode/languages"
    langResp <- callAPI lang
    langParsed <- parseLangResponse langResp
    print langParsed
 -}
    return ()

callManyContrib [] = []
callManyContrib (x:xs) = (callContribURL x):(callManyContrib xs)

test = [RepoResponse {DataTypes.id = 224238215, languages_url = "https://api.github.com/repos/melkilic/html-fundamentals-html-lists-lab-houston-web-120919/languages", contributors_url = "https://api.github.com/repos/melkilic/html-fundamentals-html-lists-lab-houston-web-120919/contributors"},RepoResponse {DataTypes.id = 224238216, languages_url = "https://api.github.com/repos/DitIsDamien/TheSilentser/languages", contributors_url = "https://api.github.com/repos/DitIsDamien/TheSilentser/contributors"}]  
 