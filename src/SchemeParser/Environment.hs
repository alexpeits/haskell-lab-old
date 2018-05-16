module SchemeParser.Environment where

import Data.IORef
import Data.Maybe (isJust)

import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.Except

import SchemeParser.Types
import SchemeParser.Error
import SchemeParser.Printer

liftThrows :: ThrowsError a -> Scheme a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: Scheme String -> IO String
runIOThrows action = extractValue <$> (trapError <$> runScheme action)

isBound :: String -> Scheme Bool
isBound var = do
  envR <- ask
  liftIO $ isJust . M.lookup var <$> readIORef envR
-- readIORef env >>= return . isJust . M.lookup var

getVar :: String -> Scheme LispVal
getVar var = do
  env <- ask >>= liftIO . readIORef
  maybe
    (throwError $ UnboundVar "Unable to get unbound variable" var)
    (liftIO . readIORef)
    (M.lookup var env)

setVar :: String -> LispVal -> Scheme LispVal
setVar var value = do
  env <- ask >>= liftIO . readIORef
  maybe
    (throwError $ UnboundVar "Unable to set unbound variable" var)
    (liftIO . flip writeIORef value)
    (M.lookup var env)
  return value

defineVar :: String -> LispVal -> Scheme LispVal
defineVar var value = do
  alreadyDefined <- isBound var
  envR <- ask
  if alreadyDefined
    then setVar var value
    else liftIO $ do valueRef <- newIORef value
                     env <- readIORef envR
                     writeIORef envR (M.insert var valueRef env)
                     return value

-- bindVars :: Env -> [(String, LispVal)] -> IO Env
-- bindVars envR bindings = readIORef envR >>= extendEnv bindings >>= newIORef
--   where extendEnv bindings env = fmap (foldr (uncurry M.insert) env) (mapM addBinding bindings)
--         addBinding (var, value) = do ref <- newIORef value
--                                      return (var, ref)
