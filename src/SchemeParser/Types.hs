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
                       closure :: M.Map String LispVal, body :: [LispVal]}
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

type Env_ = IORef (M.Map String (IORef LispVal))

data Env a = Env
  { globalEnv :: a
  , localEnv  :: a
  }

data EnvType = GlobalEnv | LocalEnv

type SchemeEnv = Env Env_

getEnv :: EnvType -> SchemeEnv -> Env_
getEnv GlobalEnv = globalEnv
getEnv LocalEnv = localEnv

setEnv :: EnvType -> SchemeEnv -> Env_ -> SchemeEnv
setEnv GlobalEnv e e_ = e {globalEnv = e_}
setEnv LocalEnv e e_ = e {localEnv = e_}

pureEnv :: Env_ -> IO (M.Map String LispVal)
pureEnv envR = do
  env <- readIORef envR
  let m = M.toList env
  return $ M.empty

nullEnv :: IO SchemeEnv
nullEnv = Env <$> newIORef M.empty <*> newIORef M.empty

testEnv :: IO SchemeEnv
testEnv = do
  envR <- nullEnv
  let e = globalEnv envR
  env <- readIORef e
  valueRef <- newIORef (LNumber 1)
  writeIORef e (M.insert "foo" valueRef env)
  return envR

-- global type

type Scheme = ReaderT SchemeEnv (ExceptT LispError IO)

runScheme :: Scheme a -> SchemeEnv -> IO (Either LispError a)
runScheme sa env = runExceptT $ runReaderT sa env

testScheme :: Scheme a -> IO (Either LispError a)
testScheme sa = do
  e <- testEnv
  runScheme sa e
