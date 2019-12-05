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
        addLangMany,
        addLang,
        fillTotalCount,
        fillLinesPerContrib,
        dbTableToJSON,
        repoFromSQL,
        contribFromSQL,
        langFromSQL,
        totalFromSQL
        ) where

import DataTypes as D
import Prelude as P
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Data.ByteString.Lazy as BL
import Data.Aeson.Encode.Pretty
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
                      \contributorsURL Text Not NULL UNIQUE)" []
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
-- final table including languages and totals for contributors and line count
    when (not ("totalCount" `P.elem` tables)) $ do
        run connection "CREATE TABLE totalCount (\
                        \language TEXT NOT NULL UNIQUE,\
                        \lineCount INTEGER NOT NULL,\
                        \contributors INTEGER NOT NULL,\
                        \lines_per_contrib FLOAT NOT NULL)" []

        return()
    commit connection

    when (not ("linesPerContrib" `P.elem` tables)) $ do
        run connection "CREATE TABLE linesPerContrib (\
                      \repoID INTEGER NOT NULL PRIMARY KEY,\
                      \line_contrib_ratio FLOAT)" []

        return()
    -- delete the derived table data as this is updated every run
    run connection "DELETE FROM totalCount" []
    run connection "DELETE FROM linesPerContrib" []
    commit connection

-- | add RepoResponses to the "repoResponses" table in the Database 
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



-- |  extract response list from Either Left/Right as it is returned by parseRepoResponse
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

-- | adds a single contributor tuple (as provided by callContribURL in the HTTP module) to the database
addContribs:: IConnection conn => conn -> (Integer,Int) -> IO()
addContribs connection tuple = handleSql handleError $ do
  run connection "INSERT OR REPLACE INTO contributorResponses (repoID, contributors) VALUES (?, ?)"
    [
      toSql (fst tuple),
      toSql (snd tuple)
    ]
  commit connection
  P.putStr "."
  where handleError e = do fail $ "error adding contributors: " ++ (show (fst tuple)) ++ " "++ (show e)

-- | adds a single language tuple (as provided by callLangURL in the HTTP module) to the database
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
  where handleError e = do fail $ "error adding contributors: " ++ (show (id)) ++ " "++ (show e)

-- | adds a list of language tuples to the database using addLang above
addLangMany :: IConnection conn => conn -> [(Integer, String, Integer)] -> IO()
addLangMany connection (x:xs) = do
  addLang connection x
  --print $ "adding to db: " ++ show x
  addLangMany connection xs
  return ()
addLangMany db _ = do
  return ()

-- | generalised function to retrieve and type convert an entire table from the database. 
-- | Returns a list of a given data type specified by the typeConverter parameter
retrieveDB:: IConnection conn => conn -> [Char] -> ([SqlValue] -> b) -> IO [b]
retrieveDB connection table typeConverter = do
        repoList <- quickQuery connection ("select * from " ++ table) []
        commit connection
        return (P.map typeConverter repoList)

-- | type converter that allows us to extract items from the database as RepoResponse objects
repoFromSQL :: [SqlValue] -> RepoResponse 
repoFromSQL [repoID, languages_url, contributors_url] =
    RepoResponse {D.id = fromSql repoID,
            languages_url = fromSql languages_url,
            contributors_url = fromSql contributors_url
    }
repoFromSQL _ = error $ "error in bytestring conversion"

-- | type converter that allows us to extract items from the database as Contributor objects
contribFromSQL :: [SqlValue] -> Contributor
contribFromSQL [repoID, contributors] =
  ContributorTo {D.repoID = fromSql repoID,
          D.contributors = fromSql contributors
  }
contribFromSQL _ = error $ "error in bytestring conversion"

-- |type converter that allows us to extract items from the database as Language objects
langFromSQL :: [SqlValue] -> Language
langFromSQL [repoID, language, lineCount] =
  LanguageTo {D.langRepoID = fromSql repoID,
          D.language = fromSql language,
          D.lineCount = fromSql lineCount
  }
langFromSQL _ = error $ "error in bytestring conversion"

-- | type converter that allows us to extract items from the database as TotalCount objects
totalFromSQL :: [SqlValue] -> TotalCount
totalFromSQL [language, lineCount, contributors, linesPerContrib] =
  TotalCount {D.totalLanguage = fromSql language,
          D.totalLineCount = fromSql lineCount,
          D.totalContributors = fromSql contributors,
          D.linesPerContrib = fromSql linesPerContrib
  }
totalFromSQL _ = error $ "error in bytestring conversion"

conrtibFromSQL [repo, lineContribRatio] =
  ContribRatio {D.repo = fromSql repo,
          D.lineContribRatio = fromSql lineContribRatio
  }
conrtibFromSQL _ = error $ "error in bytestring conversion"

-- | SQL query that aggregates across all repositories and calculates the total line count and total contributors for each language.
-- | This also populates the total count table
fillTotalCount :: IConnection conn => conn -> IO ()
fillTotalCount connection = do
  run connection
                "INSERT INTO totalCount (language, lineCount, contributors, \
                \lines_per_contrib) SELECT language, sum(contributors) as \
                \contributors,   sum(lineCount) as linecounts, \
                \(sum(lineCount) / sum(contributors)) FROM langResponses \
                \JOIN contributorResponses ON contributorResponses.repoID = \
                \langResponses.repoID GROUP BY language ORDER BY \
                \sum(lineCount) DESC" []
  commit connection

fillLinesPerContrib connection = do
  run connection
                "INSERT INTO linesPerContrib (repoID, line_contrib_ratio)\
                \SELECT lr.repoID,  SUM (lr.lineCount ) / CASE WHEN \
                \count(cr.contributors) = 0 THEN 1 ELSE count(cr.contributors) \
                \END as lineContribRatio FROM contributorResponses AS cr \
                \JOIN langResponses as lr on lr.repoID = cr.repoID \
                \GROUP BY lr.repoID \
                \ORDER BY lineContribRatio DESC" []
  commit connection

-- | Reads the repoResponses table and encodes 
repoJSONtoFile :: IConnection conn => conn -> IO ()
repoJSONtoFile db = do
  repoList <- retrieveDB db "repoResponses" repoFromSQL
  let json = BL.concat $ fmap encodePretty repoList
  BL.writeFile "repos.json" json
  print "output db to: repos.json"


-- |  
contribJSONtoFile :: IConnection conn => conn -> IO ()  
contribJSONtoFile db = do
  contribList <- retrieveDB db "contributorResponses" contribFromSQL
  let json = BL.concat $ fmap encodePretty contribList
  BL.writeFile "contributors.json" json
  print "output db to: contributor.json"


-- |
langJSONtoFile :: IConnection conn => conn -> IO ()  
langJSONtoFile db = do
  langList <- retrieveDB db "langResponses" langFromSQL
  let json = BL.concat $ fmap encodePretty langList
  BL.writeFile "languages.json" json
  print "output db to: languages.json"

-- |  

dbTableToJSON db tableName converter = do
  totalList <- retrieveDB db tableName converter
  let json = BL.concat $ fmap encodePretty totalList
  BL.writeFile (tableName ++ ".json") json
  print  $ "output db to: " ++ tableName ++ ".json"


