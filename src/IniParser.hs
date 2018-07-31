module IniParser where

import Control.Applicative hiding ((<|>), many)

import Data.Map (Map)
import qualified Data.Map as M

import Text.Parsec
import Text.Parsec.String

-- Types
newtype Header = Header String deriving (Eq, Ord, Show)

data Value = IniString String
           | IniInt Int
           -- | IniFloat Float
           | IniBool Bool
           -- | IniRef String
           deriving (Eq, Show)

newtype Name = Name String deriving (Eq, Ord, Show)
type Assignments = Map Name Value

data Section = Section Header Assignments deriving (Eq, Show)
newtype Ini = Ini (Map Header Assignments) deriving (Eq, Show)

-- Parser
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

skipComments :: Parser ()
skipComments = skipMany skipComment
  where skipComment = do
          char ';' <|> char '#'
          skipMany (noneOf "\n")
          skipEOL

parseHeader :: Parser Header
parseHeader = Header <$> (char '[' *> some letter <* char ']')

parseLitString :: Parser Value
parseLitString = IniString <$> parseString'
  where parseString' = do
          char '"'
          s <- many $ escapedChars <|> noneOf "\""
          char '"'
          return s
        escapedChars = do
          char '\\'
          c <- oneOf "\"nrt\\"
          return $ case c of
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            _   -> c

parseString :: Parser Value
parseString = IniString <$> some (noneOf "\n")

parseInt :: Parser Value
parseInt = IniInt . read <$> some digit

parseBool :: Parser Value
parseBool = IniBool . parseBool' <$> (string "true" <|> string "false")
  where parseBool' "true"  = True
        parseBool' "false" = False

parseValue :: Parser Value
parseValue = parseLitString <|> try parseBool <|> try parseInt <|> parseString

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  spaces >> char '=' >> spaces
  value <- parseValue
  return (Name name, value)

parseAssignments :: Parser Assignments
parseAssignments = M.fromList <$> some (skipWhitespace *> parseAssignment <* skipWhitespace)

parseSection :: Parser Section
parseSection = do
  skipWhitespace >> skipComments
  header <- parseHeader
  skipEOL
  assignments <- parseAssignments
  return $ Section header assignments

parseIni :: Parser Ini
parseIni = do
  sections <- some parseSection
  let sectionMap = foldr rollup M.empty sections
      rollup (Section h a) = M.insert h a
  return $ Ini sectionMap
