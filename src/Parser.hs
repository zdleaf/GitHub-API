module Parser
    ( parseRepoResponse,
      parserRepo,
      parseContribResponse,
      parserContribs,
      parseLangResponse,
    ) where

import DataTypes

-- JSON modules
import Data.Aeson as AE
import Data.Aeson.Types 

import Data.Foldable
import Control.Monad (join)

import qualified Data.HashMap.Strict as HM


parseRepoResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither parserRepoMany
    print $ "successfully decoded repo JSON"
    return decodedJSON

parseContribResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither parserContribsMany
    --print $ "successfully decoded contributors JSON"
    return decodedJSON

parseLangResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither parserLanguages
    --print $ "successfully decoded languages JSON"
    return decodedJSON

parserRepo :: Value -> Parser (Either String RepoResponse)
parserRepo value = do
    case parseEither parseJSON value of
        Left err -> return . Left $ err ++ "Invalid object is: " ++ show value
        Right parsed -> return $ Right parsed

parserRepoMany :: Value -> Parser [Either String RepoResponse]
parserRepoMany = withArray "RepoResponse" $ \arr -> do
    let allParsed = fmap (join . parseEither parserRepo) arr
    return $ toList allParsed

parserContribs :: Value -> Parser (Either String ContributorResponse)
parserContribs value = do
    case parseEither parseJSON value of
        Left err -> return . Left $ err ++ "Invalid object is: " ++ show value
        Right parsed -> return $ Right parsed

parserContribsMany :: Value -> Parser [Either String ContributorResponse]
parserContribsMany = withArray "ContributorResponse" $ \arr -> do
    let allParsed = fmap (join . parseEither parserContribs) arr
    return $ toList allParsed

parserLanguages :: Value -> Parser [LangResponse]
parserLanguages p = map (\(language, lineCount) -> LangResponse language lineCount) . HM.toList <$> parseJSON p