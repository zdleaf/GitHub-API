{-# LANGUAGE FlexibleContexts #-}

module DB
       (
        connectDB,
        initialiseDB,
        extractResp,
        addRepoMany,
        retrieveDB,
        addContribs,
        addLangMany,
        addLang,
        fillTotalCount,
        repoJSONtoFile,
        repoFromSQL,
        contribJSONtoFile,
        langJSONtoFile,
        totalJSONtoFile
        ) where

import DataTypes as D

import Prelude as P
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Data.ByteString.Lazy as BL
import Data.Aeson.Encode.Pretty


initialiseDB dbname = do
        connection <- connectSqlite3 dbname
        connectDB connection
        return connection


connectDB connection =
  do
    tables <- getTables connection
    when (not ("repoResponses" `P.elem` tables)) $ do
      run connection "CREATE TABLE repoResponses(\
                      \repoID INTEGER NOT NULL PRIMARY KEY,\
                      \languageURL TEXT NOT NULL UNIQUE,\
                      \contributorsURL Text Not NULL UNIQUE)" []
      return ()
    commit connection

    when (not ("langResponses" `P.elem` tables)) $ do
      run connection "CREATE TABLE langResponses (\
                      \repoID INTEGER NOT NULL,\
                      \language TEXT NOT NULL,\
                      \lineCount INTEGER NOT NULL,\
                      \CONSTRAINT limitdb UNIQUE (repoID, language))" []
      return()
    commit connection

    when (not ("contributorResponses" `P.elem` tables)) $ do
      run connection "CREATE TABLE contributorResponses (\
                      \repoID INTEGER NOT NULL PRIMARY KEY,\
                      \contributors INTEGER NOT NULL)" []

      return()
    commit connection

-- final table including languages and totals for contributors and line count
    when (not ("totalCount" `P.elem` tables)) $ do
        run connection "CREATE TABLE totalCount (\
                        \language TEXT NOT NULL UNIQUE,\
                        \lineCount INTEGER NOT NULL,\
                        \contributors INTEGER NOT NULL)" []

        return()
    commit connection

--addRepo :: Connection -> Reporesponse -> IO ()
addRepo connection (Left err) = return ()
addRepo connection (Right repoResponse) = handleSql handleError $ do

        run connection "INSERT OR REPLACE INTO repoResponses (repoID,\
                       \languageURL, contributorsURL) VALUES (?, ?, ?)"
            [
              toSql (D.id repoResponse),
              toSql (languages_url repoResponse),
              toSql (contributors_url repoResponse)
            ]
        commit connection
        where handleError e = do fail $ "error adding repo: " ++ (show (D.id repoResponse)) ++ " "++ (show e)

-- extract response list from Either Left/Right
extractResp (Left err) = []
extractResp (Right list) = list

addRepoMany :: IConnection t => t -> [Either String RepoResponse] -> IO ()
addRepoMany db (x:xs) = do
    addRepo db x
    addRepoMany db xs
    return ()
addRepoMany db _ = do
    return ()

addContribs connection tuple = handleSql handleError $ do
  run connection "INSERT OR REPLACE INTO contributorResponses (repoID, contributors) VALUES (?, ?)"
    [
      toSql (fst tuple),
      toSql (snd tuple)
    ]
  commit connection
  P.putStr "."
  where handleError e = do fail $ "error adding contributors: " ++ (show (fst tuple)) ++ " "++ (show e)

addLang connection (id, language, count)  = handleSql handleError $ do
  run connection "INSERT OR REPLACE INTO langResponses (repoID, language, lineCount) VALUES (?, ?, ?)"
    [
      toSql id,
      toSql language,
      toSql count
    ]
  commit connection
  P.putStr "."
  where handleError e = do fail $ "error adding contributors: " ++ (show (id)) ++ " "++ (show e)

addLangMany connection (x:xs) = do
  addLang connection x
  --print $ "adding to db: " ++ show x
  addLangMany connection xs
  return ()
addLangMany db _ = do
  return ()

retrieveDB connection table typeConverter = do
        repoList <- quickQuery connection ("select * from "++table) []
        commit connection
        return (P.map typeConverter repoList)

repoFromSQL [repoID, languages_url, contributors_url] =
    RepoResponse {D.id = fromSql repoID,
            languages_url = fromSql languages_url,
            contributors_url = fromSql contributors_url
    }
repoFromSQL _ = error $ "error in bytestring conversion"

contribFromSQL [repoID, contributors] =
  ContributorTo {D.repoID = fromSql repoID,
          D.contributors = fromSql contributors
  }
contribFromSQL _ = error $ "error in bytestring conversion"

langFromSQL [repoID, language, lineCount] =
  LanguageTo {D.langRepoID = fromSql repoID,
          D.language = fromSql language,
          D.lineCount = fromSql lineCount
  }
langFromSQL _ = error $ "error in bytestring conversion"

totalFromSQL [language, lineCount, contributors] =
  TotalCount {D.totalLanguage = fromSql language,
          D.totalLineCount = fromSql lineCount,
          D.totalContributors = fromSql contributors
  }
totalFromSQL _ = error $ "error in bytestring conversion"

fillTotalCount connection = do
  run connection "INSERT INTO totalCount (language, contributors,\
                            \lineCount) SELECT language, sum(contributors) \
                            \as contributors, sum(lineCount) as lineCount FROM \
                            \langResponses JOIN contributorResponses ON \
                            \contributorResponses.repoID = \
                            \langResponses.repoID GROUP BY language ORDER BY \
                            \sum(lineCount) DESC" []
  commit connection

repoJSONtoFile db = do
  repoList <- retrieveDB db "repoResponses" repoFromSQL
  let json = BL.concat $ fmap encodePretty repoList
  BL.writeFile "repos.json" json
  print "output db to: repos.json"

contribJSONtoFile db = do
  contribList <- retrieveDB db "contributorResponses" contribFromSQL
  let json = BL.concat $ fmap encodePretty contribList
  BL.writeFile "contributors.json" json
  print "output db to: contributor.json"

langJSONtoFile db = do
  langList <- retrieveDB db "langResponses" langFromSQL
  let json = BL.concat $ fmap encodePretty langList
  BL.writeFile "languages.json" json
  print "output db to: languages.json"

totalJSONtoFile db = do
  totalList <- retrieveDB db "totalCount" totalFromSQL
  let json = BL.concat $ fmap encodePretty totalList
  BL.writeFile "totalcounts.json" json
  print "output db to: totalcounts.json"