module SchemeParser.Environment where

import GHC.IO.Unsafe
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

-- runIOThrows :: Scheme String -> IO String
-- runIOThrows action = extractValue <$> (trapError <$> runScheme action)

isBound :: EnvType -> String -> Scheme Bool
isBound et var = do
  envR <- asks (getEnv et)
  liftIO $ isJust . M.lookup var <$> readIORef envR
-- readIORef env >>= return . isJust . M.lookup var

getVar :: EnvType -> String -> Scheme LispVal
getVar et var = do
  env <- asks (getEnv et) >>= liftIO . readIORef
  maybe
    (throwError $ UnboundVar "Unable to get unbound variable" var)
    (liftIO . readIORef)
    (M.lookup var env)

setVar :: EnvType -> String -> LispVal -> Scheme LispVal
setVar et var value = do
  env <- asks (getEnv et) >>= liftIO . readIORef
  maybe
    (throwError $ UnboundVar "Unable to set unbound variable" var)
    (liftIO . flip writeIORef value)
    (M.lookup var env)
  return value

defineVar :: EnvType -> String -> LispVal -> Scheme LispVal
defineVar et var value = do
  alreadyDefined <- isBound et var
  envR <- asks (getEnv et)
  if alreadyDefined
    then setVar et var value
    else liftIO $ do valueRef <- newIORef value
                     env <- readIORef envR
                     writeIORef envR (M.insert var valueRef env)
                     return value

bindVars' :: EnvType -> SchemeEnv -> [(String, LispVal)] -> IO SchemeEnv
bindVars' et env bindings = setEnv et env <$> (readIORef (getEnv et env) >>= extendEnv bindings >>= newIORef)
  where extendEnv bindings env' = fmap (foldr (uncurry M.insert) env') (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

bindVars :: EnvType -> [(String, LispVal)] -> Scheme SchemeEnv
bindVars et bindings = do
  e <- ask
  envR <- asks (getEnv et)
  env <- liftIO $ readIORef envR
  b <- liftIO $ extendEnv bindings env
  liftIO $ setEnv et e <$> newIORef b
  where extendEnv bindings env = fmap (foldr (uncurry M.insert) env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

-- degbugging
instance (Show a) => Show (IORef a) where
    show a = show (unsafePerformIO (readIORef a))
