module DB
    ( connectDB,
      initialiseDB
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3

--type dbname = String
--initialiseDB :: dbname ->
initialiseDB dbname = do
    connection <- connectSqlite3 dbname
    commit connection
    connectDB connection
    return connection


connectDB connection = do
    tables <- getTables connection
    if not ("Reporesponses" `elem` tables) then do
     run connection "CREATE TABLE Reporesponses (\
                    \gitID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                    \repoURl TEXT NOT NULL UNIQUE)" []
     commit connection
     return ()
    else return ()

    return connection

