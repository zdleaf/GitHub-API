module Parser
    ( parseResponse
    ) where

import DataTypes

-- JSON modules
import Data.Aeson as AE
import Data.Aeson.Types
--import Data.Aeson.TH

import Data.Text
import Data.Foldable
import Control.Monad (join)

parseResponse response = do
    print $ "successfully decoded JSON"
    let decodedJSON = eitherDecode response >>= parseEither verboseParseMany
    return decodedJSON

{- change/rewrite the below functions - came from https://geekingfrog.com/blog/post/struggles-with-parsing-json-with-aeson -}
verboseParser :: Value -> Parser (Either String Reporesponse)
verboseParser v = do
    case parseEither parseJSON v of
        Left err -> return . Left $ err ++ " -- Invalid object is: " ++ show v
        Right parsed -> return $ Right parsed

verboseParseMany :: Value -> Parser [Either String Reporesponse]
verboseParseMany = withArray "Reporesponse" $ \arr -> do
    let allParsed = fmap (join . parseEither verboseParser) arr
    return $ toList allParsed
    