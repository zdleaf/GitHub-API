module Parser
    ( parseResponse,
      verboseParser,
      amendContributorsURL
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

-- we want to change https://api.github.com/repos/owner/repo/contributors to repo/stats/contributors
amendContributorsURL url = (reverse $ snd (break (=='/') (reverse url))) ++ "stats/contributors"

{- change/rewrite the below functions - came from https://geekingfrog.com/blog/post/struggles-with-parsing-json-with-aeson -}
verboseParser :: FromJSON (a) => Value -> Parser (Either String a)
verboseParser value = do
    case parseEither parseJSON value of
        Left err -> return . Left $ err ++ "Invalid object is: " ++ show value
        Right parsed -> return $ Right parsed

verboseParseMany :: FromJSON (a) => Value -> Parser [Either String a]
verboseParseMany = withArray "RepoResponse" $ \arr -> do
    let allParsed = fmap (join . parseEither verboseParser) arr
    return $ toList allParsed

parseLanguages :: Value -> Parser [LangResponse]
parseLanguages =
  -- We're expecting an object: {"languageName": 1234}
  withObject "languages" $ \o ->
    -- Now we have 'o', which is a HashMap. We can use HM.toList to turn it
    -- into a list of pairs (domain, referer) and then parse each referer:
    for (HM.toList o) $ \(language, lineCount) -> do
      -- accesses :: [(Text, Int)]
      accesses <- HM.toList <$> parseJSON referer
      -- accesses' :: [(String, Int)]
      let accesses' = map (\(page, n) -> (T.unpack page, n)) accesses
      return $ Referer {
        domain       = T.unpack domain,
        pathAccesses = accesses' }