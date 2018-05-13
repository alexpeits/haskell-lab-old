module SchemeParser.Parser where

import Numeric (readInt, readFloat, readHex, readOct)
import Data.Ratio
import Data.Complex
import Data.Char (digitToInt)
import Data.Maybe (listToMaybe, fromJust)

import qualified Data.Map as M

import Data.Functor (($>))

import Text.Parsec hiding (spaces)
import Text.Parsec.Prim

import SchemeParser.Types

-- parser helpers

symbol :: LispParser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: LispParser ()
spaces = skipMany1 space

escapedChars :: LispParser Char
escapedChars = do
  char '\\'
  c <- oneOf "\"nrt\\"
  return $ case c of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _   -> c

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

--
-- Lisp value parsers
--

-- primitives

parseAtom :: LispParser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ LAtom atom

parseString :: LispParser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\""
  char '"'
  return $ LString x

parseChar :: LispParser LispVal
parseChar = do
  try (string "#\\")
  c <- try (string "newline" <|> string "space")
       <|> do { x <- anyChar; notFollowedBy alphaNum; return [x]}
  return $ LChar $ case c of
    "newline" -> '\n'
    "space"   -> ' '
    _         -> head c

parseBool :: LispParser LispVal
parseBool = 
  try (string "#t" $> LBool True) <|> try (string "#f" $> LBool False)

-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
parseNumberRadix :: LispParser LispVal
parseNumberRadix =
  parseBin <|> parseOct <|> parseDec <|> parseHex
  where
    parseBin = do
      try (string "#b")
      LNumber . fromJust . readBin <$> many1 (oneOf "01")
    parseOct = do
      try (string "#o")
      LNumber . fst . (!! 0) . readOct <$> many1 octDigit
    parseDec = do
      try (string "#d")
      parseNumber
    parseHex = do
      try (string "#x")
      LNumber . fst . (!! 0) . readHex <$> many1 hexDigit

parseNumber :: LispParser LispVal
parseNumber = parseNumberRadix <|> LNumber . read <$> many1 digit

parseFloat :: LispParser LispVal
parseFloat = do
  l <- many1 digit
  char '.'
  r <- many1 digit
  return $ LFloat $ fst $ head $ readFloat (l ++ "." ++ r)

parseRatio :: LispParser LispVal
parseRatio = do
  n <- many1 digit
  char '/'
  d <- many1 digit
  return $ LRatio (read n % read d)

parseComplex :: LispParser LispVal
parseComplex = do
  r <- try parseFloat <|> parseNumber
  char '+'
  i <- try parseFloat <|> parseNumber
  char 'i'
  return $ LComplex (toDouble r :+ toDouble i)

-- lists

parseList :: LispParser LispVal
parseList = LList <$> sepBy parseExpr spaces

parseDottedList :: LispParser LispVal
parseDottedList = do
  hd <- endBy parseExpr spaces
  tl <- char '.' >> spaces >> parseExpr
  return $ LDottedList hd tl

parseQuoted :: LispParser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ LList [LAtom "quote", x]

parseQuasiQuoted :: LispParser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ LList [LAtom "quasiquote", x]

parseUnquoted :: LispParser LispVal
parseUnquoted = do
  char ','
  x <- parseExpr
  return $ LList [LAtom "unquote", x]

parseUnquoteSpliced :: LispParser LispVal
parseUnquoteSpliced = do
  try $ string ",@"
  x <- parseExpr
  return $ LList [LAtom "unquote-splice", x]

parseVector :: LispParser LispVal
parseVector = LVector <$> sepBy parseExpr spaces

--
-- helpers
--

toDouble :: LispVal -> Double
toDouble (LFloat f) = realToFrac f
toDouble (LNumber n) = fromIntegral n

--

parseExpr :: LispParser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseChar
            <|> try parseFloat
            <|> try parseRatio
            <|> try parseComplex
            <|> parseNumber
            <|> parseBool
            <|> parseQuoted
            <|> parseQuasiQuoted
            <|> parseUnquoteSpliced
            <|> parseUnquoted
            <|> char '(' *> (try parseList <|> parseDottedList) <* char ')'
            <|> try (string "#(" *> parseVector <* char ')')
