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
addRepo connection repoResponse =
    do
        run connection "INSERT INTO Reporesponses (gitID, languageURL, contributorsURL )\
                           \VALUES (?, ?, ?)"
                           [
                           (toSql (D.id repoResponse)),
                           (toSql (languages_url repoResponse)),
                           (toSql (contributors_url repoResponse))
                            ]
        commit connection
        return ()

-- extract response either Left/Right in reponse list
extractResp (Left _) = []
extractResp (Right list) = list

addRepoMany db responseList = fmap (addRepo db) responseList

{-
extractResponseList :: Either [Char] [Either String D.Reporesponse] -> Either (IO (), [Either String D.Reporesponse])      
extractResponseList response =
    case response of
    Left str -> print $ "error Left in extractResponseList: " ++ str
    Right list -> return list

addRepoMany [] db = print $ "empty list"
addRepoMany (x:xs) db = do
    case x
    
    if extractResp x == 
    | x == Left str = do
        print $ "error Left in addRepoMany: " ++ str
        addRepoMany xs
    | x == Right repo = do
        addRepo db repo
        addRepoMany xs



addRepoMany [] db = print $ "empty list"
addRepoMany (x:xs) db = if extractResp x == 
    | x == Left str = do
        print $ "error Left in addRepoMany: " ++ str
        addRepoMany xs
    | x == Right repo = do
        addRepo db repo
        addRepoMany xs

        
addRepoMany [] db = []
addRepoMany (x:xs) db =
    case x of
        Left str -> do
            print $ "error Left in addRepoMany: " ++ str
            addRepoMany xs db
        Right repo -> do
            addRepo db repo
            addRepoMany xs db
            -}