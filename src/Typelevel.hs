{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Typelevel where

import qualified Data.Text as T
import Data.Kind
import Control.Exception

import Data.String (IsString, fromString)
import Control.Monad.Writer

instance a ~ () => IsString (Writer String a) where
  fromString = tell

main :: Writer String ()
main = do
  "hello" :: Writer String ()
  "world"  :: Writer String ()

foo = execWriter (do "hello"; "world" :: Writer String ())
