module SchemeParser.Scheme where

import Data.Map (empty)
import System.Environment (getArgs)

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M

import qualified Text.Parsec as P

import SchemeParser.Types
import SchemeParser.Error
import SchemeParser.Parser
import SchemeParser.Eval
import SchemeParser.Environment
import SchemeParser.Printer

readExpr :: String -> Scheme LispVal
readExpr input = case P.parse parseExpr "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

evalAndPrint :: SchemeEnv -> String -> IO ()
evalAndPrint env s = evalExpr env s >>= putStrLn

evalExpr :: SchemeEnv -> String -> IO String
evalExpr env expr = do
  res <- runExceptT $ runReaderT (readExpr expr >>= eval) env
  return $ extractValue $ trapError $ fmap show res

envWithPrims :: IO SchemeEnv
envWithPrims = nullEnv >>= (\x -> bindVars' GlobalEnv x (map makePrimFunc (M.toList primitives)))
  where makePrimFunc (var, func) = (var, LPrimFunc func)

main :: IO ()
main = do
  env <- testEnv
  args <- getArgs
  evalAndPrint env $ head args
