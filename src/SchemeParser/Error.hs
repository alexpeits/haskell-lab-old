module SchemeParser.Error where

import Control.Monad.Except

import Text.Parsec (ParseError)

import SchemeParser.Types

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: Either LispError a -> a
extractValue (Right val) = val
