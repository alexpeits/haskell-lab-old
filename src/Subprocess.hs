module Subprocess where

import Data.List
import Control.Arrow (second)

splitSpace :: String -> (String, String)
splitSpace = second tail . span (/= ' ')

ls :: IO ()
ls = do
  pkgs <- readFile "/home/alex/.cabal/hackage.txt"
  let pkgs' = map splitSpace (lines pkgs)
  print $ map last $ groupBy (\a b -> fst a == fst b) pkgs'
