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
import Prelude as P
import Data.Aeson.Types 
import Data.ByteString.Lazy as BL
import Data.Foldable
import Control.Monad (join)

import qualified Data.HashMap.Strict as HM

-- | Decodes raw JSON byteString from the repositories API call into a list of RepoResponse objects.
parseRepoResponse:: ByteString-> IO (Either String [Either String RepoResponse])
parseRepoResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither parserRepoMany
    print $ "successfully decoded repo JSON"
    return decodedJSON


-- | Decodes raw JSON byteString from the contributor API call into a list of Contributor objects.
parseContribResponse :: Monad m => ByteString -> m (Either String [Either String Contributor])
parseContribResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither parserContribsMany
    return decodedJSON


-- | Decodes raw JSON byteString from the Language API call into a list of Language objects.
parseLangResponse:: Monad m => ByteString -> m (Either String [Language])
parseLangResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither parserLanguages
    return decodedJSON


-- | Parses a single decoded JSON value into an Either RepoResponse object.
parserRepo :: Value -> Parser (Either String RepoResponse)
parserRepo value = do
    case parseEither parseJSON value of
        Left err -> return . Left $ err ++ "Invalid object is: " ++ show value
        Right parsed -> return $ Right parsed


-- | Maps parserRepo to an array of JSON values and returns a list of the parsed RepoResponse objects.      
parserRepoMany :: Value -> Parser [Either String RepoResponse]
parserRepoMany = withArray "RepoResponse" $ \arr -> do
    let allParsed = fmap (join . parseEither parserRepo) arr
    return $ toList allParsed


-- | Parses a single decoded JSON value into an Either Contributor object.   
parserContribs :: Value -> Parser (Either String Contributor)
parserContribs value = do
    case parseEither parseJSON value of
        Left err -> return . Left $ err ++ "Invalid object is: " ++ show value
        Right parsed -> return $ Right parsed


-- | Maps parserContribs to an array of JSON values and returns a list of the parsed Contributor objects.        
parserContribsMany :: Value -> Parser [Either String Contributor]
parserContribsMany = withArray "Contributor" $ \arr -> do
    let allParsed = fmap (join . parseEither parserContribs) arr
    return $ toList allParsed


-- | As the languages JSON had a varying number of unknown fields, albeit in a structured format,
-- Our parser creates a list of Language objects for each repository.  
parserLanguages :: Value -> Parser [Language]
parserLanguages p = P.map (\(language, lineCount) -> LanguageFrom language lineCount) . HM.toList <$> parseJSON p