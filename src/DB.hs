{-# LANGUAGE FlexibleContexts #-}


module DB
       (
        connectDB,
        initialiseDB,
        extractResp,
        addRepoMany,
        retrieveRepoResponse
        ) where

import DataTypes as D
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad

--type dbname = String
--initialiseDB :: dbname ->
initialiseDB dbname = do
        connection <- connectSqlite3 dbname
        connectDB connection
        return connection

connectDB connection =
  do
    tables <- getTables connection
    when (not ("repoResponses" `elem` tables)) $ do
      run connection "CREATE TABLE repoResponses(\
                      \repoID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                      \languageURL TEXT NOT NULL UNIQUE,\
                      \contributorsURL Text Not NULL UNIQUE)" []
      return ()
    commit connection


    when (not ("langResponses" `elem` tables)) $ do
      run connection "CREATE TABLE langResponses (\
                      \repoID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                      \language TEXT NOT NULL UNIQUE,\
                      \lineCount INTEGER NOT NULL UNIQUE)" []
      return()
    commit connection

    when (not ("contributorResponses" `elem` tables)) $ do
      run connection "CREATE TABLE contributorResponses (\
                      \repoID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                      \contributors INTEGER NOT NULL UNIQUE)" []

      return()
    commit connection

-- final table that of language and totals for contributors and count
    when (not ("totalCount" `elem` tables)) $ do
        run connection "CREATE TABLE totalCount (\
                        \language TEXT NOT NULL UNIQUE,\
                        \lineCount INTEGER NOT NULL UNIQUE,\
                        \contributors INTEGER NOT NULL UNIQUE)" []

        return()
    commit connection

-- INSERT INTO Reporesponses

-- SELECT 2, "test", "test"
-- WHERE NOT EXISTS  (SELECT 1 from Reporesponses WHERE repoID = 224238003)

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
  run connection "INSERT OR REPLACE INTO contributorResponses (repoID,\
                 \contributors) VALUES (?, ?)"
    [
      toSql (fst tuple),
      toSql (snd tuple)
    ]
  commit connection
  where handleError e = do fail $ "error adding contributors: \
                          \" ++ (show (fst tuple)) ++ " "++ (show e)

retrieveRepoResponse connection = do
        urls <- quickQuery connection "select repoID, languageURL, \
                                      \contributorsURL from repoResponses" []
        commit connection
        return (map fromSqlurls urls)

fromSqlurls [repoID, languages_url, contributors_url] =
    RepoResponse {D.id = fromSql repoID,
            languages_url = fromSql languages_url,
            contributors_url = fromSql contributors_url
    }
fromSqlurls _ = error $ "error in bytestring conversion"

{- TRY map FMAP, sequence, mapM
--addRepoMany db responseList = fmap (addRepo db) responseList

extractRepo (Left err) = do
		print $ "extractRepo Left: " ++ err
		return Reporesponse {}
extractRepo (Right repo) = repo
-}
