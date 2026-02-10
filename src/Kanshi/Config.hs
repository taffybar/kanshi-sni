module Kanshi.Config
  ( parseKanshiConfig
  , getProfileNames
  , getProfileNamesFrom
  ) where

import qualified Control.Exception as E
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Void
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Skip whitespace and comments
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parse a quoted or unquoted name
profileName :: Parser Text
profileName = quotedName <|> unquotedName
  where
    quotedName = do
      q <- char '"' <|> char '\''
      name <- takeWhileP Nothing (/= q)
      void $ char q
      return name
    unquotedName =
      takeWhile1P (Just "profile name") (\c -> c /= '{' && c /= '}' && c /= ' ' && c /= '\t' && c /= '\n' && c /= '\r')

-- Skip everything inside braces (handles nesting)
bracedBlock :: Parser ()
bracedBlock = do
  void $ lexeme $ char '{'
  skipManyTill (bracedBlock <|> void (satisfy (/= '}'))) (char '}')
  sc

-- Parse a profile declaration, return its name
profileDecl :: Parser (Maybe Text)
profileDecl = do
  void $ lexeme $ string "profile"
  name <- lexeme profileName
  bracedBlock
  return $ Just name

-- Skip any other top-level construct
otherDecl :: Parser (Maybe Text)
otherDecl = choice
  [ do void $ lexeme $ string "output"
       void $ takeWhileP Nothing (\c -> c /= '{' && c /= '\n')
       bracedBlock
       return Nothing
  , do void $ lexeme $ string "include"
       void $ takeWhileP Nothing (/= '\n')
       return Nothing
  , do bracedBlock  -- anonymous profile
       return Nothing
  ]

-- Top-level parser
kanshiConfig :: Parser [Text]
kanshiConfig = do
  sc
  results <- many (profileDecl <|> otherDecl)
  eof
  return [name | Just name <- results]

-- | Parse kanshi config text and return profile names
parseKanshiConfig :: Text -> Either String [Text]
parseKanshiConfig input =
  case parse kanshiConfig "<kanshi config>" input of
    Left err -> Left $ errorBundlePretty err
    Right names -> Right names

-- | Read and parse a kanshi config file at the given path
getProfileNamesFrom :: FilePath -> IO (Either String [Text])
getProfileNamesFrom configPath = do
  result <- E.try $ TIO.readFile configPath
  case (result :: Either E.IOException Text) of
    Left err -> return $ Left $ "Could not read config: " ++ show err
    Right content -> return $ parseKanshiConfig content

-- | Read and parse the default kanshi config file
getProfileNames :: IO (Either String [Text])
getProfileNames = do
  configDir <- getXdgDirectory XdgConfig "kanshi"
  getProfileNamesFrom (configDir </> "config")
