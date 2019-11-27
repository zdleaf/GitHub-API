module DB
    ( connectDB,
      initialiseDB,
      extractResp,
      addRepoMany
    ) where

import DataTypes as D

import Database.HDBC
import Database.HDBC.Sqlite3

import Control.Monad (mzero)

--type dbname = String
--initialiseDB :: dbname ->
initialiseDB dbname = do
    connection <- connectSqlite3 dbname
    connectDB connection
    return connection


connectDB connection = do
    tables <- getTables connection
    if not ("Reporesponses" `elem` tables) then do
     run connection "CREATE TABLE Reporesponses (\
                    \gitID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                    \languageURL TEXT NOT NULL UNIQUE,\
                    \contributorsURL Text Not NULL UNIQUE)" []
     commit connection

     return ()

    else return ()


test_response = (Reporesponse 1 "test" "test")

--addRepo :: Connection -> Reporesponse -> IO ()
addRepo connection (Left err) = return ()
addRepo connection (Right repoResponse) = do
    run connection "INSERT INTO Reporesponses (gitID, languageURL, contributorsURL )\
                        \VALUES (?, ?, ?)"
                        [
                        (toSql (D.id repoResponse)),
                        (toSql (languages_url repoResponse)),
                        (toSql (contributors_url repoResponse))
                        ]
    commit connection
    return ()

-- extract response list from Either Left/Right
extractResp (Left err) = []
extractResp (Right list) = list

addRepoMany :: IConnection t => t -> [Either String Reporesponse] -> IO b
addRepoMany db (x:xs) = do
    addRepo db x
    addRepoMany db xs

{-
--addRepoMany db responseList = fmap (addRepo db) responseList

extractRepo (Left err) = do
    print $ "extractRepo Left: " ++ err
    return Reporesponse {}
extractRepo (Right repo) = repo
-}