{-# LANGUAGE FlexibleInstances #-}
module Variadic where


class SumN r where
  sumN :: Int -> r

instance SumN Int where
  sumN = id

instance SumN r => SumN (Int -> r) where
  sumN x = sumN . (x +)
