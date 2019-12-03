module Parser
    ( parseRepoResponse,
      parserRepo,
      amendContributorsURL,
      parseContribResponse,
      parserContribs
    ) where

import DataTypes

-- JSON modules
import Data.Aeson as AE
import Data.Aeson.Types

import Data.Foldable
import Control.Monad (join)

parseRepoResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither parseRepoMany
    print $ "successfully decoded JSON"
    return decodedJSON


parseContribResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither parserContribsMany
    print $ "successfully decoded JSON"
    return decodedJSON


-- we want to change https://api.github.com/repos/owner/repo/contributors to repo/stats/contributors
amendContributorsURL url = (reverse $ snd (break (=='/') (reverse url))) ++ "stats/contributors"


{- change/rewrite the below functions - came from https://geekingfrog.com/blog/post/struggles-with-parsing-json-with-aeson -}
parserRepo :: Value -> Parser (Either String RepoResponse)
parserRepo value = do
    case parseEither parseJSON value of
        Left err -> return . Left $ err ++ "Invalid object is: " ++ show value
        Right parsed -> return $ Right parsed


parseRepoMany :: Value -> Parser [Either String RepoResponse]
parseRepoMany = withArray "RepoResponse" $ \arr -> do
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

