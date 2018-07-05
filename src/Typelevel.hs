{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Typelevel where

import Data.String (IsString, fromString)
import Control.Monad.Writer

instance a ~ () => IsString (Writer String a) where
  fromString = tell

main :: Writer String ()
main = do
  "hello" :: Writer String ()
  "world"  :: Writer String ()

foo = execWriter (do "hello"; "world" :: Writer String ())
