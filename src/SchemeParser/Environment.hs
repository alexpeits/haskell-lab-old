module SchemeParser.Environment where

import Data.IORef
import Data.Maybe (isJust)

import qualified Data.Map as M

import Control.Monad.Except

import SchemeParser.Types
import SchemeParser.Error
import SchemeParser.Printer

type IOThrowsError = ExceptT LispError IO

nullEnv :: IO Env
nullEnv = newIORef M.empty

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

isBound :: Env -> String -> IO Bool
isBound envR var = isJust . M.lookup var <$> readIORef envR
-- readIORef env >>= return . isJust . M.lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envR var = do
  env <- liftIO $ readIORef envR
  maybe
    (throwError $ UnboundVar "Unable to get unbound variable" var)
    (liftIO . readIORef)
    (M.lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envR var value = do
  env <- liftIO $ readIORef envR
  maybe
    (throwError $ UnboundVar "Unable to set unbound variable" var)
    (liftIO . flip writeIORef value)
    (M.lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envR var value = do
  alreadyDefined <- liftIO $ isBound envR var
  if alreadyDefined
    then setVar envR var value
    else liftIO $ do valueRef <- newIORef value
                     env <- readIORef envR
                     writeIORef envR (M.insert var valueRef env)
                     return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envR bindings = readIORef envR >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = fmap (foldr (uncurry M.insert) env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)
