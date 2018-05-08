module Parsing where

import Text.Parsec hiding (spaces)
import Text.Parsec.Prim

import qualified Data.Map as M

type LispParser = Parsec String (M.Map String LispVal)

data LispVal = LAtom String
             | LList [LispVal]
             | LDottedList [LispVal] LispVal
             | LNumber Integer
             | LString String
             | LBool Bool
             deriving Show

symbol :: LispParser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: LispParser ()
spaces = skipMany1 space

parseString :: LispParser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ LString x

parseAtom :: LispParser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
             "#t" -> LBool True
             "#f" -> LBool False
             _    -> LAtom atom

parseNumber :: LispParser LispVal
parseNumber = LNumber . read <$> many1 digit

--

parseExpr :: LispParser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber

readExpr :: String -> String
readExpr input = case runParser parseExpr M.empty "lisp" input of
  Left err   -> "No match: " ++ show err
  Right val  -> "Found value: " ++ show val
