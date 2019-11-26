--module Githubdb where

import Database.HDBC
import Database.HDBC.Sqlite3


connectDb :: FilePath -> IO Connection
connectDb filepath =
    do
        connection <- connectSqlite3 "github.db"

        tables <- getTables connection
        if "Reporesponses" `elem` tables then
            do run connection "CREATE TABLE Reporesponses (\
                        \gitHubID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                        \repoURl TEXT NOT NULL UNIQUE)" []
        else return ()

       return connection