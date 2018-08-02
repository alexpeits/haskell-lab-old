{-# LANGUAGE InstanceSigs #-}
module HaskellBook.State where

import Control.Monad

-- import Control.Monad.State

import qualified Data.DList as DL

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \x ->
    let (a, s) = g x in (f a, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \x -> (a, x)

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \x ->
    let (t, s)  = f x
        (a, s') = g x
    in (t a, s')

instance Monad (Moi s) where
  return :: a -> Moi s a
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi $ \x ->
    let (a, s) = f x
        moi = g a
    in runMoi moi s

-- fizzbuzz

-- fizzBuzz :: Integer -> String
-- fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
--            | n `mod` 5  == 0 = "Buzz"
--            | n `mod` 3  == 0 = "Fizz"
--            | otherwise       = show n
-- 
-- -- fizzbuzzList :: [Integer] -> [String]
-- -- fizzbuzzList list = execState (mapM_ addResult list) []
-- 
-- -- fizzbuzzList :: [Integer] -> [String]
-- fizzbuzzList :: [Integer] -> DL.DList String
-- fizzbuzzList list =
--   -- let dlist =
--     execState (mapM_ addResult list) DL.empty
--   -- in DL.apply dlist []
-- 
-- -- addResult :: Integer -> State [String] ()
-- -- addResult n = do
-- --   xs <- get
-- --   let result = fizzBuzz n
-- --   put (result : xs)
-- 
-- addResult :: Integer -> State (DL.DList String) ()
-- addResult n = do
--   xs <- get
--   let result = fizzBuzz n
--   put (DL.snoc xs result)
-- 
-- fizzMain :: IO ()
-- fizzMain = mapM_ putStrLn $ fizzbuzzList [1..100]

get :: Moi s s
get = Moi $ \x -> (x, x)

put :: s -> Moi s ()
put s = Moi $ const ((), s)

exec :: Moi s a -> s -> s
exec (Moi sa) = snd . sa

eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \x -> ((), f x)
