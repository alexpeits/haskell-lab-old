{-# LANGUAGE InstanceSigs #-}
module Fldbl where

import Data.Monoid
import Data.Foldable

import Data.Functor.Reverse

-- fold :: Monoid m => t m -> m
-- fold = foldMap id

-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- foldMap f = foldr (mappend . f) mempty

-- foldr :: (a -> b -> b) -> b -> t a -> b
-- foldr f z t = appEndo (foldMap (Endo #. f) t) z

-- foldr' :: (a -> b -> b) -> b -> t a -> b
-- foldr' f z0 xs = foldl f' id xs z0
    -- where f' k x z = k $! f x z

-- foldl :: (b -> a -> b) -> b -> t a -> b
-- foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z

-- foldl' :: (b -> a -> b) -> b -> t a -> b
-- foldl' f z0 xs = foldr f' id xs z0
    -- where f' x k z = k $! f z x

-- null :: t a -> Bool
-- null = foldr (\_ _ -> False) True

-- length :: t a -> Int
-- length = foldl' (\c _ -> c+1) 0

-- elem :: Eq a => a -> t a -> Bool
-- elem = any . (==)

-- sum :: Num a => t a -> a
-- sum = getSum #. foldMap Sum

-- product :: Num a => t a -> a
-- product = getProduct #. foldMap Product

-- toList :: t a -> [a]
-- maximum :: forall a . Ord a => t a -> a
-- minimum :: forall a . Ord a => t a -> a
-- foldr1 :: (a -> a -> a) -> t a -> a
-- foldl1 :: (a -> a -> a) -> t a -> a


data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving Show

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf         = mempty
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

myTree :: Tree Char
myTree = Node (Node Leaf 'a' Leaf) 'b' (Node Leaf 'c' Leaf)
