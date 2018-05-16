module SchemeParser.Types where

import Data.IORef
import Data.Complex (Complex)
import Data.Ratio (Rational)

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M

import Text.Parsec

-- parser

type LispParser = Parsec String (M.Map String LispVal)

data LispVal = LAtom String
             | LString String
             | LBool Bool
             | LChar Char
             | LNumber Integer
             | LFloat Float
             | LRatio Rational
             | LComplex (Complex Double)
             | LList [LispVal]
             | LDottedList [LispVal] LispVal
             | LVector [LispVal]
             deriving Eq

-- errors

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

-- environment

type Env = IORef (M.Map String (IORef LispVal))

nullEnv :: IO Env
nullEnv = newIORef M.empty

testEnv :: IO Env
testEnv = do
  envR <- nullEnv
  env <- readIORef envR
  valueRef <- newIORef (LNumber 1)
  writeIORef envR (M.insert "foo" valueRef env)
  return envR

-- global type

type Scheme = ReaderT Env (ExceptT LispError IO)

runScheme :: Scheme a -> IO (Either LispError a)
runScheme sa = do
  env <- nullEnv
  runExceptT $ runReaderT sa env
