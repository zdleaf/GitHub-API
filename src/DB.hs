{-# LANGUAGE FlexibleContexts #-}
-- | Database Operations
module DB
       (
        connectDB,
        initialiseDB,
        extractResp,
        addRepoMany,
        retrieveDB,
        addContribs,
        addLangList,
        addLang,
        fillTotalCount,
        fillLinesPerContrib,
        dbTableToJSON,
        repoFromSQL,
        contribFromSQL,
        langFromSQL,
        totalFromSQL,
        avgContribFromSQL,
        topThreeLang,
        printTopThreeLang
        ) where

import DataTypes as D
import Prelude as P

import System.IO
import Control.Monad (when)

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.ByteString.Lazy as BL
import Data.Aeson.Encode.Pretty

import Data.Aeson.Types

-- | Initialise the database with a given path name
initialiseDB :: FilePath -> IO Connection
initialiseDB dbname = do
        connection <- connectSqlite3 dbname
        connectDB connection
        return connection

-- | Connect the database and create all the tables
connectDB :: IConnection conn => conn -> IO ()
connectDB connection =
  do
    tables <- getTables connection
    when (not ("repoResponses" `P.elem` tables)) $ do
      run connection "CREATE TABLE repoResponses(\
                      \repoID INTEGER NOT NULL PRIMARY KEY,\
                      \languageURL TEXT NOT NULL UNIQUE,\
                      \contributorsURL Text NOT NULL UNIQUE)" []
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
-- Final table including languages and totals for contributors and line count
    when (not ("totalCount" `P.elem` tables)) $ do
        run connection "CREATE TABLE totalCount (\
                        \language TEXT NOT NULL UNIQUE,\
                        \lineCount INTEGER NOT NULL,\
                        \contributors INTEGER NOT NULL,\
                        \linesPerContrib REAL NOT NULL)" []

        return()
    commit connection

    when (not ("linesPerContrib" `P.elem` tables)) $ do
        run connection "CREATE TABLE linesPerContrib (\
                      \repoID INTEGER NOT NULL PRIMARY KEY,\
                      \avgLinesPerContrib REAL)" []

        return()
    -- Delete data from the the derived tables as this is updated every run
    run connection "DELETE FROM totalCount" []
    run connection "DELETE FROM linesPerContrib" []
    commit connection

-- | Add RepoResponses to the "repoResponses" table in the Database
addRepo:: IConnection conn => conn -> Either a RepoResponse -> IO ()
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



-- | Extract response list from Either Left/Right as it is returned by parseRepoResponse
extractResp :: Either a1 [a2] -> [a2]
extractResp (Left err) = []
extractResp (Right list) = list

-- | Recursively calls addRepo on a list of Either RepoResponse objects
addRepoMany :: IConnection t => t -> [Either String RepoResponse] -> IO ()
addRepoMany db (x:xs) = do
    addRepo db x
    addRepoMany db xs
    return ()
addRepoMany db _ = do
    return ()

-- | Adds a single contributor tuple (as provided by callContribURL in the HTTP module) to the database
addContribs:: IConnection conn => conn -> (Integer,Int) -> IO()
addContribs connection tuple = handleSql handleError $ do
  run connection "INSERT OR REPLACE INTO contributorResponses (repoID, contributors) VALUES (?, ?)"
    [
      toSql (fst tuple),
      toSql (snd tuple)
    ]
  commit connection
  P.putStr "."
  hFlush stdout
  where handleError e = do fail $ "error adding contributors: " ++ (show (fst tuple)) ++ " "++ (show e)

-- | Adds a single language tuple (as provided by callLangURL in the HTTP module) to the database
addLang :: IConnection conn => conn -> (Integer, String, Integer) -> IO()
addLang connection (id, language, count)  = handleSql handleError $ do
  run connection "INSERT OR REPLACE INTO langResponses (repoID, language, lineCount) VALUES (?, ?, ?)"
    [
      toSql id,
      toSql language,
      toSql count
    ]
  commit connection
  P.putStr "."
  hFlush stdout
  where handleError e = do fail $ "error adding contributors: " ++ (show (id)) ++ " "++ (show e)

-- | Adds a list of language tuples to the database using addLang above
addLangList :: IConnection conn => conn -> [(Integer, String, Integer)] -> IO()
addLangList connection (x:xs) = do
  addLang connection x
  --print $ "adding to db: " ++ show x
  addLangList connection xs
  return ()
addLangList db _ = do
  return ()

-- | Generalised function to retrieve and type convert an entire table from the database.
--  Returns a list of a given data type specified by the typeConverter parameter
retrieveDB:: IConnection conn => conn -> [Char] -> ([SqlValue] -> b) -> IO [b]
retrieveDB connection table typeConverter = do
        repoList <- quickQuery connection ("select * from " ++ table) []
        commit connection
        return (P.map typeConverter repoList)

-- | Type converter that allows us to extract items from the database as RepoResponse objects
repoFromSQL :: [SqlValue] -> RepoResponse
repoFromSQL [repoID, languages_url, contributors_url] =
    RepoResponse {D.id = fromSql repoID,
            languages_url = fromSql languages_url,
            contributors_url = fromSql contributors_url
    }
repoFromSQL _ = error $ "error in bytestring conversion"

-- | Type converter that allows us to extract items from the database as Contributor objects
contribFromSQL :: [SqlValue] -> Contributor
contribFromSQL [repoID, contributors] =
  ContributorTo {D.repoID = fromSql repoID,
          D.contributors = fromSql contributors
  }
contribFromSQL _ = error $ "error in bytestring conversion"

-- |Type converter that allows us to extract items from the database as Language objects
langFromSQL :: [SqlValue] -> Language
langFromSQL [repoID, language, lineCount] =
  LanguageTo {D.langRepoID = fromSql repoID,
          D.language = fromSql language,
          D.lineCount = fromSql lineCount
  }
langFromSQL _ = error $ "error in bytestring conversion"

-- | Type converter that allows us to extract items from the database as TotalCount objects
totalFromSQL :: [SqlValue] -> TotalCount
totalFromSQL [language, lineCount, contributors, linesPerContrib] =
  TotalCount {D.totalLanguage = fromSql language,
          D.totalLineCount = fromSql lineCount,
          D.totalContributors = fromSql contributors,
          D.linesPerContrib = fromSql linesPerContrib
  }
totalFromSQL _ = error $ "error in bytestring conversion"

avgContribFromSQL [repo, avgLinesPerContrib] =
  AvgContribLines {D.repo = fromSql repo,
          D.avgLinesPerContrib = fromSql avgLinesPerContrib
  }
avgContribFromSQL _ = error $ "error in bytestring conversion"

-- | SQL query that aggregates across all repositories and calculates the total line count and total contributors for each language.
--  This also populates the total count table
fillTotalCount :: IConnection conn => conn -> IO ()
fillTotalCount connection = do
  run connection
                "INSERT INTO totalCount (language, lineCount, contributors,\
                \linesPerContrib) SELECT language, sum(lineCount), \
                \sum(contributors),  (1.0*sum(lineCount) / CASE WHEN \
                \sum(cr.contributors) IN (NULL, 0) THEN 1 ELSE \
                \sum(cr.contributors) END ) as linesPerContrib FROM \
                \langResponses JOIN contributorResponses cr ON cr.repoID = \
                \langResponses.repoID GROUP BY language ORDER BY \
                \ sum(lineCount) DESC" []
  commit connection

-- | SQL query that calculates the average number  of lines per contributor for each repository.
-- | This query then populates the LinesPerContrib table
fillLinesPerContrib :: IConnection conn => conn -> IO ()
fillLinesPerContrib connection = do
  run connection
                "INSERT INTO linesPerContrib (repoID, avgLinesPerContrib)\
                \SELECT lr.repoID,  SUM (1.0*lr.lineCount ) / CASE WHEN \
                \sum(cr.contributors) IN (NULL, 0) THEN 1 ELSE sum(cr.contributors) \
                \END as LinesPerContrib FROM contributorResponses AS cr \
                \JOIN langResponses as lr on lr.repoID = cr.repoID \
                \GROUP BY lr.repoID \
                \ORDER BY LinesPerContrib DESC" []
  commit connection

-- | Takes a table name and its conveter (FromSQL), ecncodes the the list then writes it out to a JSON file matching it's table name
dbTableToJSON :: (IConnection conn, ToJSON b )=> conn -> [Char] -> ([SqlValue] -> b) -> IO ()
dbTableToJSON db tableName converter = do
  totalList <- retrieveDB db tableName converter
  let json = BL.concat $ fmap encodePretty totalList
  BL.writeFile (tableName ++ ".json") json
  print  $ "output db to: " ++ tableName ++ ".json"


-- | Finds 3 repositories with the largest number of lines per contributor
-- topThreeLang :: IConnection conn => conn -> IO []
topThreeLang connection = do
  topThree <- quickQuery connection "SELECT * FROM totalCount ORDER BY \
                                    \linesPerContrib DESC LIMIT 3" []
  commit connection
  print "The three repositories with the largest number of lines per \
        \ contributor are: "
  (printTopThreeLang (P.map totalFromSQL topThree))

printTopThreeLang (x:xs) = do
  print ((D.totalLanguage x),(D.linesPerContrib x))
  printTopThreeLang xs
printTopThreeLang _ = print ("")

