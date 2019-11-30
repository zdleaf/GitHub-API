module DB
       (
        connectDB,
        initialiseDB,
        extractResp,
        addRepoMany,
        retrieveLanguageUrls,
        retrieveContributorUrls
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
      when (not ("Reporesponses" `elem` tables)) $ do
        run connection "CREATE TABLE Reporesponses(\
                        \gitID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                        \languageURL TEXT NOT NULL UNIQUE,\
                        \contributorsURL Text Not NULL UNIQUE)" []
        return ()
      commit connection

      when (not ("repoData" `elem` tables)) $ do
        run connection "CREATE TABLE repoData (\
                       \gitID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \language TEXT NOT NULL UNIQUE,\
                       \languageCOUNT INTEGER NOT NULL UNIQUE,\
                       \collaborators TEXT NOT NULL UNIQUE,\
                       \collaboratorsCOUNT INTEGER NOT NULL UNIQUE)" []
        return ()
      commit connection

--addRepo :: Connection -> Reporesponse -> IO ()
addRepo connection (Left err) = return ()
addRepo connection (Right repoResponse) = handleSql handleError $ do
        run connection "INSERT INTO Reporesponses (gitID, languageURL, contributorsURL )\
                                                \VALUES (?, ?, ?)"
                                                [
                                                (toSql (D.id repoResponse)),
                                                (toSql (languages_url repoResponse)),
                                                (toSql (contributors_url repoResponse))
                                                ]
        commit connection
        where handleError e = do fail $ "error adding podcase - does it exist. \
												 \PodcastId = " ++ (show (D.id repoResponse)) ++ (show e)

-- extract response list from Either Left/Right
extractResp (Left err) = []
extractResp (Right list) = list

addRepoMany :: IConnection t => t -> [Either String Reporesponse] -> IO ()
addRepoMany db (x:xs) = do
    addRepo db x
    addRepoMany db xs
    return ()
addRepoMany db _ = do
    return ()

retrieveLanguageUrls connection = do
        urls <- quickQuery connection "select gitID, languageURL from Reporesponses" []
        commit connection
        return (map fromSqlurls urls)


retrieveContributorUrls connection = do
        urls <- quickQuery connection "select gitID, contributorsURL from Reporesponses" []
        commit connection
        return (map fromSqlurls urls)


fromSqlurls [gitId, gitURL] =
    Urlobj {gitID = fromSql gitId,
            url = fromSql gitURL
    }
fromSqlurls _ = error $ "error in bytestring conversion"

{- TRY map FMAP, sequence, mapM
--addRepoMany db responseList = fmap (addRepo db) responseList

extractRepo (Left err) = do
		print $ "extractRepo Left: " ++ err
		return Reporesponse {}
extractRepo (Right repo) = repo
-}
