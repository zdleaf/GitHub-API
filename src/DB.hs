module DB
    ( connectDB,
      initialiseDB
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import DataTypes as D
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
                    \languageURL TEXT NOT NULL UNIQUE)" []
     commit connection

     return ()

    else return ()


test_response = Reporesponse 1 "test"
--addRepo :: Connection -> Reporesponse -> IO ()
addRepo repoResponse =
    do
        connection <- connectSqlite3 "github.db"
        let repoLanguagesUrl = languages_url repoResponse
        let repoId = D.id repoResponse

        run connection "INSERT INTO Reporesponses (gitID, repoURL)\
                           \VALUES (?, ?)" [toSql repoId repoLanguagesUrl]
        commit connection
        return ()