module Kanshi.Config
  ( parseKanshiConfig
  , getProfileNames
  , getProfileNamesFrom
  , ProfileSpec(..)
  , OutputSpec(..)
  , getProfiles
  , getProfilesFrom
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

data OutputSpec = OutputSpec
  { outputTarget :: Text
  , outputEnabled :: Maybe Bool
  , outputMode :: Maybe Text
  , outputPosition :: Maybe (Int, Int)
  , outputScale :: Maybe Double
  } deriving (Show, Eq)

data ProfileSpec = ProfileSpec
  { profileSpecName :: Text
  , profileSpecOutputs :: [OutputSpec]
  } deriving (Show, Eq)

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

signedInt :: Parser Int
signedInt = L.signed sc L.decimal

-- Parse a single output statement inside a profile block.
--
-- Example:
--   output "BOE ..." enable mode 2560x1600@240.00Hz position 0,0 scale 1.0
outputStmt :: Parser OutputSpec
outputStmt = do
  void $ lexeme $ string "output"
  target <- lexeme profileName
  let go spec =
        choice
          [ do void $ lexeme $ string "enable"
               go spec { outputEnabled = Just True }
          , do void $ lexeme $ string "disable"
               go spec { outputEnabled = Just False }
          , do void $ lexeme $ string "mode"
               m <- lexeme (takeWhile1P (Just "mode") (\c -> c /= ' ' && c /= '\t' && c /= '\n' && c /= '\r' && c /= '}'))
               go spec { outputMode = Just m }
          , do void $ lexeme $ string "position"
               x <- lexeme signedInt
               void $ char ','
               y <- lexeme signedInt
               sc
               go spec { outputPosition = Just (x, y) }
          , do void $ lexeme $ string "scale"
               s <- lexeme L.float
               go spec { outputScale = Just s }
          , do -- Ignore any other token on the line.
               _ <- lexeme (takeWhile1P Nothing (\c -> c /= ' ' && c /= '\t' && c /= '\n' && c /= '\r' && c /= '}'))
               go spec
          , pure spec
          ]
  go OutputSpec
    { outputTarget = target
    , outputEnabled = Nothing
    , outputMode = Nothing
    , outputPosition = Nothing
    , outputScale = Nothing
    }

profileSpecBody :: Parser [OutputSpec]
profileSpecBody = do
  void $ lexeme $ char '{'
  let bodyItem =
        sc *> choice
          [ Just <$> try outputStmt
          , Nothing <$ try bracedBlock
          , Nothing <$ void (some (satisfy (\c -> c /= '{' && c /= '}')))
          ]
  results <- manyTill bodyItem (char '}')
  sc
  pure [o | Just o <- results]

profileSpecDecl :: Parser ProfileSpec
profileSpecDecl = do
  void $ lexeme $ string "profile"
  name <- lexeme profileName
  outputs <- profileSpecBody
  pure $ ProfileSpec name outputs

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

-- | Parse kanshi config text and return structured profile specs.
parseKanshiProfiles :: Text -> Either String [ProfileSpec]
parseKanshiProfiles input =
  let item =
        choice
          [ Just <$> try profileSpecDecl
          , Nothing <$ try otherDecl
          , Nothing <$ void anySingle
          ]
  in case parse (sc *> many item <* eof) "<kanshi config>" input of
    Left err -> Left $ errorBundlePretty err
    Right specs -> Right [s | Just s <- specs]

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

getProfilesFrom :: FilePath -> IO (Either String [ProfileSpec])
getProfilesFrom configPath = do
  result <- E.try $ TIO.readFile configPath
  case (result :: Either E.IOException Text) of
    Left err -> return $ Left $ "Could not read config: " ++ show err
    Right content -> return $ parseKanshiProfiles content

getProfiles :: IO (Either String [ProfileSpec])
getProfiles = do
  configDir <- getXdgDirectory XdgConfig "kanshi"
  getProfilesFrom (configDir </> "config")
