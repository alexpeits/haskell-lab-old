{-# LANGUAGE FlexibleInstances #-}
module SchemeParser.Types where

import Data.IORef
import Data.Complex (Complex)
import Data.Ratio (Rational)

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M

import Text.Parsec

-- parser

type LispParser = Parsec String ()

data LispVal = LAtom String
             | LNil
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
             | LPrimFunc ([LispVal] -> Scheme LispVal)
             | LFunc { params :: [String], vararg :: Maybe String,
                       body :: [LispVal]}
             deriving Eq


instance Eq ([LispVal] -> Scheme LispVal) where
  _ /= _ = False

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

runScheme :: Scheme a -> Env -> IO (Either LispError a)
runScheme sa env = runExceptT $ runReaderT sa env

testScheme :: Scheme a -> IO (Either LispError a)
testScheme sa = do
  e <- testEnv
  runScheme sa e
