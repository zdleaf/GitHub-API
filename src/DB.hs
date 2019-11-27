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
                    \languageURL TEXT NOT NULL UNIQUE,\
                    \contributorsURL Text Not NULL UNIQUE)" []
     commit connection

     return ()

    else return ()


test_response = (Reporesponse 1 "test" "test")


--addRepo :: Connection -> Reporesponse -> IO ()
addRepo repoResponse =
    do
        connection <- connectSqlite3 "github.db"

        run connection "INSERT INTO Reporesponses (gitID, languageURL, contributorsURL )\
                           \VALUES (?, ?, ?)"
                           [
                           (toSql (D.id repoResponse)),
                           (toSql (languages_url repoResponse)),
                           (toSql (contributors_url repoResponse))
                            ]
        commit connection
        return ()

        