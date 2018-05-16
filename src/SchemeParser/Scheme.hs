module SchemeParser.Scheme where

import Data.Map (empty)
import System.Environment (getArgs)

import Control.Monad.Reader
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

readExpr :: String -> Scheme LispVal
readExpr input = case runParser parseExpr empty "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env s = evalExpr env s >>= putStrLn

evalExpr :: Env -> String -> IO String
evalExpr env expr = do
  res <- runExceptT $ runReaderT (readExpr expr >>= eval) env
  return $ extractValue $ trapError $ fmap show res

main :: IO ()
main = do
  env <- testEnv
  args <- getArgs
  evalAndPrint env $ head args
