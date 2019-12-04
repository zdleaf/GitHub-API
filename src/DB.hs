{-# LANGUAGE FlexibleContexts #-}

module DB
       (
        connectDB,
        initialiseDB,
        extractResp,
        addRepoMany,
        retrieveRepoResponse,
        addContribs,
        addContribsMany,
        addLangMany
        ) where

import DataTypes as D
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad


initialiseDB dbname = do
        connection <- connectSqlite3 dbname
        connectDB connection
        return connection


connectDB connection =
  do
    tables <- getTables connection
    when (not ("repoResponses" `elem` tables)) $ do
      run connection "CREATE TABLE repoResponses(\
                      \repoID INTEGER NOT NULL PRIMARY KEY,\
                      \languageURL TEXT NOT NULL UNIQUE,\
                      \contributorsURL Text Not NULL UNIQUE)" []
      return ()
    commit connection


    when (not ("langResponses" `elem` tables)) $ do
      run connection "CREATE TABLE langResponses (\
                      \repoID INTEGER NOT NULL,\
                      \language TEXT NOT NULL,\
                      \lineCount INTEGER NOT NULL,\
                      \CONSTRAINT limitdb UNIQUE (repoID, language))" []
      return()
    commit connection

    when (not ("contributorResponses" `elem` tables)) $ do
      run connection "CREATE TABLE contributorResponses (\
                      \repoID INTEGER NOT NULL PRIMARY KEY,\
                      \contributors INTEGER NOT NULL)" []

      return()
    commit connection

-- final table including languages and totals for contributors and line count
    when (not ("totalCount" `elem` tables)) $ do
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
  where handleError e = do fail $ "error adding contributors: " ++ (show (fst tuple)) ++ " "++ (show e)

addContribsMany db (x:xs) = do
  addContribs db x
  print $ "adding to db: " ++ show x
  addContribsMany db xs
  return ()
addContribsMany db _ = do
  return ()

addLang connection (id, language, count)  = handleSql handleError $ do
  run connection "INSERT OR REPLACE INTO langResponses (repoID, language, lineCount) VALUES (?, ?, ?)"
    [
      toSql id,
      toSql language,
      toSql count
    ]
  commit connection
  where handleError e = do fail $ "error adding contributors: " ++ (show (id)) ++ " "++ (show e)

addLangMany connection (x:xs) = do
  addLang connection x
  print $ "adding to db: " ++ show x
  addLangMany connection xs
  return ()
addLangMany db _ = do
  return ()

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



