{-# LANGUAGE FlexibleContexts #-}
-- | Database Operations
module DB
       (
        connectDB,
        initialiseDB,
        extractResp,
        addRepo,
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
        topFiveLangs,
        topFiveContribs,
        topLinesPerContrib,
        printResults,
        printContribResults,
        printAvgLPCResults,
        retrieveRepoBetween
        ) where

import DataTypes as D
import Prelude as P

import System.IO
import Control.Monad (when)

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.ByteString.Lazy as BL
import Data.Aeson.Encode.Pretty
import Data.Convertible.Base

import Data.Aeson.Types

-- | Initialise the database with a given path name.
initialiseDB :: FilePath -> IO Connection
initialiseDB dbname = do
        connection <- connectSqlite3 dbname
        connectDB connection
        return connection

-- | Connect to the database and create all the tables.
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
-- Final table including languages and totals for contributors and line count.
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

-- | Add RepoResponses to the "repoResponses" table in the database. 
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
        where handleError e = do print $ "error adding repo: " ++ (show (D.id repoResponse)) ++ " "++ (show e)

-- | Extract response list from Either Left/Right as returned by parseRepoResponse.
extractResp :: Either a1 [a2] -> [a2]
extractResp (Left err) = []
extractResp (Right list) = list

-- | Recursively calls addRepo on a list of Either Error RepoResponse objects.
addRepoMany :: IConnection conn => conn -> [Either String RepoResponse] -> IO ()
addRepoMany db (x:xs) = do
    addRepo db x
    addRepoMany db xs
    return ()
addRepoMany db _ = do
    return ()

-- | Adds a single contributor tuple (as provided by callContribURL in the HTTP module) to the database.
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
  where handleError e = do print $ "error adding contributors: " ++ (show (fst tuple)) ++ " "++ (show e)

-- | Adds a single language tuple (as provided by callLangURL in the HTTP module) to the database.
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
  where handleError e = do print $ "error adding contributors: " ++ (show (id)) ++ " "++ (show e)

-- | A recursive function adding a list of language tuples to the database using addLang above.
addLangList :: IConnection conn => conn -> [(Integer, String, Integer)] -> IO()
addLangList connection (x:xs) = do
  addLang connection x
  --print $ "adding to db: " ++ show x
  addLangList connection xs
  return ()
addLangList db _ = do
  return ()

-- | Generalised function to retrieve and type convert an entire table from the database SQL format to Haskell data types.
--  Returns a list of a given data type specified by the typeConverter parameter.
retrieveDB:: IConnection conn => conn -> [Char] -> ([SqlValue] -> b) -> IO [b]
retrieveDB connection table typeConverter = do
        repoList <- quickQuery connection ("select * from " ++ table) []
        commit connection
        return (P.map typeConverter repoList)

-- | Retrieve the repos from the DB between the requested start and end repoID. This is so we call only the newly added languages_url and contributors_url for repos specified in the current run. This allows us to build up a database over time of many repos, language and contributor responses, without duplicating API calls, given we can only make 5000 API calls/hour. 
retrieveRepoBetween :: (IConnection conn, Convertible a SqlValue) => conn -> a -> a -> IO [RepoResponse]
retrieveRepoBetween connection start end = do
  repoList <- quickQuery connection ("select * from repoResponses \
                                      \WHERE repoID > (?) AND repoID < (?)\
                                      \")
                                      [
                                      toSql start,
                                      toSql end
                                      ]
  commit connection
  return (P.map repoFromSQL repoList)

{- SQL CONVERTERS -}

-- | Type converter that allows us to extract items from the database as RepoResponse objects.
repoFromSQL :: [SqlValue] -> RepoResponse
repoFromSQL [repoID, languages_url, contributors_url] =
    RepoResponse {D.id = fromSql repoID,
            languages_url = fromSql languages_url,
            contributors_url = fromSql contributors_url
    }
repoFromSQL _ = error $ "error in bytestring conversion"

-- | Type converter that allows us to extract items from the database as Contributor objects.
contribFromSQL :: [SqlValue] -> Contributor
contribFromSQL [repoID, contributors] =
  ContributorTo {D.repoID = fromSql repoID,
          D.contributors = fromSql contributors
  }
contribFromSQL _ = error $ "error in bytestring conversion"

-- | Type converter that allows us to extract items from the database as Language objects.
langFromSQL :: [SqlValue] -> Language
langFromSQL [repoID, language, lineCount] =
  LanguageTo {D.langRepoID = fromSql repoID,
          D.language = fromSql language,
          D.lineCount = fromSql lineCount
  }
langFromSQL _ = error $ "error in bytestring conversion"

-- | Type converter that allows us to extract items from the database as TotalCount objects.
totalFromSQL :: [SqlValue] -> TotalCount
totalFromSQL [language, lineCount, contributors, linesPerContrib] =
  TotalCount {D.totalLanguage = fromSql language,
          D.totalLineCount = fromSql lineCount,
          D.totalContributors = fromSql contributors,
          D.linesPerContrib = fromSql linesPerContrib
  }
totalFromSQL _ = error $ "error in bytestring conversion"

-- | Type converter that allows us to extract items from the database as AvgContribLines objects.
avgContribFromSQL:: [SqlValue] -> AvgContribLines
avgContribFromSQL [repo, avgLinesPerContrib] =
  AvgContribLines {D.repo = fromSql repo,
          D.avgLinesPerContrib = fromSql avgLinesPerContrib
  }
avgContribFromSQL _ = error $ "error in bytestring conversion"

-- | SQL query that aggregates across all repositories and calculates the total line count and total contributors for each language.
--  This also populates the total count table.
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
-- | This query then populates the LinesPerContrib table.
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

-- | Takes a table name and its conveter (FromSQL), ecncodes the the list then writes it out to a JSON file matching it's table name.
dbTableToJSON :: (IConnection conn, ToJSON b )=> conn -> [Char] -> ([SqlValue] -> b) -> IO ()
dbTableToJSON db tableName converter = do
  totalList <- retrieveDB db tableName converter
  let json = BL.concat $ fmap encodePretty totalList
  BL.writeFile ("./outputJSON/" ++ tableName ++ ".json") json
  print  $ "output db to: " ++ tableName ++ ".json"

-- | Finds and prints the top languages from the totalCount table by a number of metrics (line count, most contributors, most average lines per contributor).
topFiveLangs:: IConnection conn => conn -> IO ()
topFiveLangs connection = do
  -- line counts
  result <- quickQuery connection "SELECT language, lineCount, contributors, round(linesPerContrib, 2) FROM totalCount ORDER BY lineCount DESC LIMIT 5" []
  commit connection
  P.putStrLn "\nThe five languages with the highest line counts are: "
  printResults "lineCount" (P.map totalFromSQL result)

  -- contributors
  result <- quickQuery connection "SELECT language, lineCount, contributors, round(linesPerContrib, 2) FROM totalCount ORDER BY contributors DESC LIMIT 5" []
  commit connection
  P.putStrLn "\nThe five languages with the most contributors are: "
  printResults "contribs" (P.map totalFromSQL result)

  -- average line count per contributor
  result <- quickQuery connection "SELECT language, lineCount, contributors, round(linesPerContrib, 2) FROM totalCount ORDER BY linesPerContrib DESC LIMIT 3" []
  commit connection
  P.putStrLn "\nThe three languages with the largest average number of lines per contributor are: "
  printResults "avglpc" (P.map totalFromSQL result)

-- | Finds and prints the top repos by contributors from the contributorResponses table.
topFiveContribs::IConnection conn => conn -> IO ()
topFiveContribs connection = do
  result <- quickQuery connection "SELECT * FROM contributorResponses ORDER BY contributors DESC LIMIT 5" []
  commit connection
  P.putStrLn "\nThe five repositories with the largest number of contributors are: "
  printContribResults (P.map contribFromSQL result)

-- | Finds and prints the top repos by average lines per contributor from the linesPerContrib table.
topLinesPerContrib:: IConnection conn => conn -> IO ()
topLinesPerContrib connection = do
  result <- quickQuery connection "SELECT * FROM linesPerContrib\
                                    \ ORDER BY avgLinesPerContrib DESC LIMIT \
                                    \5" []
  commit connection
  P.putStrLn "\nThe five repositories with the highest number of lines per contributor are: "
  printAvgLPCResults (P.map avgContribFromSQL result)

-- | When printing the top five results from the totalCount at the end of program execution, this function gets the relevant fields to display from the DataTypes and prints them.
printResults :: [Char] -> [TotalCount] -> IO ()
printResults z (x:xs)
  | z == "lineCount" = do
    print ((D.totalLanguage x),(D.totalLineCount x))
    printResults "lineCount" xs
  | z == "contribs" = do
    print ((D.totalLanguage x),(D.totalContributors x))
    printResults "contribs" xs
  | z == "avglpc" = do
    print ((D.totalLanguage x),(D.linesPerContrib x))
    printResults "avglpc" xs
printResults z _ = return ()

-- | Gets the relevant fields and prints them for the top 5 repo's by contributor.
printContribResults :: [Contributor] -> IO ()
printContribResults (x:xs) = do
    print ((D.repoID x),(D.contributors x))
    printContribResults xs
printContribResults _ = return ()

-- | Gets the relevant fields and prints them for the top 5 repo's by average lines per contributor.
printAvgLPCResults :: [AvgContribLines] -> IO ()
printAvgLPCResults (x:xs) = do
    print ((D.repo x),(D.avgLinesPerContrib x))
    printAvgLPCResults xs
printAvgLPCResults _ = return ()