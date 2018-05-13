module SchemeParser.Scheme where

import Data.Map (empty)
import System.Environment (getArgs)

import Control.Monad.Except

import Text.Parsec (runParser)

import SchemeParser.Types
import SchemeParser.Error
import SchemeParser.Parser
import SchemeParser.Eval
import SchemeParser.Printer

readExpr' :: String -> String
readExpr' input = case runParser parseExpr empty "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

readExpr :: String -> ThrowsError LispVal
readExpr input = case runParser parseExpr empty "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

evalAndPrint :: String -> IO ()
evalAndPrint = putStrLn . evalExpr

evalExpr :: String -> String
evalExpr expr = extractValue $ fmap show $ readExpr expr >>= eval

main :: IO ()
main = getArgs >>= evalAndPrint . head
