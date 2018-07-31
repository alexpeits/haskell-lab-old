{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module HaskellBook.ParserCombinators where

import Data.List (inits)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Control.Applicative

import Text.Trifecta
import Text.Trifecta.Combinators

import Text.RawString.QQ

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

oneTwoThreeAll :: Parser String
oneTwoThreeAll = choice $ map string $ reverse $ tail $ inits "123"

string' :: String -> Parser String
string' = mapM char

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
  skipMany (string "\n")
  >>
  (Left <$> integer) <|> (Right <$> some letter)

eitherOr :: String
eitherOr = [r|
123
abc
456
def|]

-- ini parser

ini :: String
ini = [r|
; comment
[section]
host=wikipedia.org
alias=claw
|]

newtype Header = Header String deriving (Eq, Ord, Show)

newtype Name = Name String deriving (Eq, Ord, Show)
newtype Value = Value String deriving (Eq, Ord, Show)
type Assignments = Map Name Value

data Section = Section Header Assignments deriving (Eq, Show)
newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
  char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  spaces >> char '=' >> spaces
  val <- some (noneOf "\n")
  skipEOL
  return (Name name, Value val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments = skipMany skipComment
  where skipComment = do
          char ';' <|> char '#'
          skipMany (noneOf "\n")
          skipEOL

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections =
        foldr rollup M.empty sections
      rollup (Section h a) = M.insert h a
  return $ Config mapOfSections
