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

-- | 
parseRepoResponse:: ByteString-> IO (Either String [Either String RepoResponse])
parseRepoResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither parserRepoMany
    print $ "successfully decoded repo JSON"
    return decodedJSON

-- |   
parseContribResponse :: Monad m => ByteString -> m (Either String [Either String Contributor])
parseContribResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither parserContribsMany
    --print $ "successfully decoded contributors JSON"
    return decodedJSON

-- |    
parseLangResponse:: Monad m => ByteString -> m (Either String [Language])
parseLangResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither parserLanguages
    --print $ "successfully decoded languages JSON"
    return decodedJSON


-- |    
parserRepo :: Value -> Parser (Either String RepoResponse)
parserRepo value = do
    case parseEither parseJSON value of
        Left err -> return . Left $ err ++ "Invalid object is: " ++ show value
        Right parsed -> return $ Right parsed


-- |        
parserRepoMany :: Value -> Parser [Either String RepoResponse]
parserRepoMany = withArray "RepoResponse" $ \arr -> do
    let allParsed = fmap (join . parseEither parserRepo) arr
    return $ toList allParsed


-- |    
parserContribs :: Value -> Parser (Either String Contributor)
parserContribs value = do
    case parseEither parseJSON value of
        Left err -> return . Left $ err ++ "Invalid object is: " ++ show value
        Right parsed -> return $ Right parsed


-- |        
parserContribsMany :: Value -> Parser [Either String Contributor]
parserContribsMany = withArray "Contributor" $ \arr -> do
    let allParsed = fmap (join . parseEither parserContribs) arr
    return $ toList allParsed


-- |    
parserLanguages :: Value -> Parser [Language]
parserLanguages p = P.map (\(language, lineCount) -> LanguageFrom language lineCount) . HM.toList <$> parseJSON p