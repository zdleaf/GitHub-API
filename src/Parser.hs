module Parser
    ( parseResponse,
      verboseParser
    ) where

import DataTypes

-- JSON modules
import Data.Aeson as AE
import Data.Aeson.Types

import Data.Foldable
import Control.Monad (join)

parseResponse response = do
    let decodedJSON = eitherDecode response >>= parseEither verboseParseMany
    print $ "successfully decoded JSON"
    return decodedJSON

{- change/rewrite the below functions - came from https://geekingfrog.com/blog/post/struggles-with-parsing-json-with-aeson -}
verboseParser :: Value -> Parser (Either String Reporesponse)
verboseParser value = do
    case parseEither parseJSON value of
        Left err -> return . Left $ err ++ "Invalid object is: " ++ show value
        Right parsed -> return $ Right parsed

verboseParseMany :: Value -> Parser [Either String Reporesponse]
verboseParseMany = withArray "Reporesponse" $ \arr -> do
    let allParsed = fmap (join . parseEither verboseParser) arr
    return $ toList allParsed
