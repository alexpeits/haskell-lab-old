{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Dependently.Vec where

import Data.Kind
import Dependently.Nat

-- Length-indexed vectors
data Vec :: Type -> Nat -> Type where
  VNil :: Vec a Zero
  (:>) :: a -> Vec a n -> Vec a (Succ n)

infixr 5 :>

instance (Show a) => Show (Vec a n) where
  show v = "[" ++ go v
    where go :: (Show a) => Vec a n -> String
          go v = case v of
            VNil      -> "]"
            (x :> xs) -> show x ++ sep ++ go xs
              where sep = case xs of
                      VNil -> ""
                      _    -> ", "

vnil = VNil :: Vec Int Zero
x = 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> VNil
y = 7 :> 8 :> 9 :> 10 :> VNil

vecSum :: Vec Int n -> Int
vecSum VNil = 0
vecSum (x :> xs) = x + vecSum xs

(+++) :: Vec a n -> Vec a m -> Vec a (n + m)
VNil      +++ ys = ys
(x :> xs) +++ ys = x :> (xs +++ ys)

(!!!) :: Vec a n -> Fin n -> a
vec !!! fin = case (fin, vec) of
  (FZ, x :> _)    -> x
  (FS n, _ :> xs) -> xs !!! n

-- Various operations
vhead :: Vec a (Succ n) -> a
vhead (x :> _) = x

vlast :: Vec a (Succ n) -> a
vlast (x :> xs)   = case xs of
  VNil     -> x
  (_ :> _) -> vlast xs

vtail :: Vec a (Succ n) -> Vec a n
vtail (_ :> xs) = xs

vinit :: Vec a (Succ n) -> Vec a n
vinit (x :> xs) = vinit' x xs
  where vinit' :: a -> Vec a n -> Vec a n
        vinit' _ VNil      = VNil
        vinit' y (z :> zs) = y :> vinit' z zs

vnull :: Vec a n -> Bool
vnull VNil = True
vnull _    = False

-- | needs ScopedTypeVariables
-- class Peano (n :: Nat) where peano :: Int
-- instance Peano Zero where peano = 0
-- instance (Peano n) => Peano (Succ n) where peano = 1 + peano @n
--
-- vlength :: forall a n. (Peano n) => Vec a n -> Int
-- vlength _ = peano @n

vlength :: Vec a n -> Int
vlength VNil      = 0
vlength (_ :> xs) = 1 + vlength xs

vmap :: (a -> b) -> Vec a n -> Vec b n
vmap _ VNil      = VNil
vmap f (a :> as) = f a :> vmap f as

-- vreverse :: Vec a n -> Vec a n
-- vreverse VNil = VNil
-- vreverse l@(x :> xs) = case xs of
  -- VNil -> l
  -- _    -> vreverse xs +++ (x :> VNil)


-- vintersperse :: a -> Vec a (Succ (Succ n)) -> Vec a (E ((Succ (Succ n) ** Two) - One))
-- vintersperse :: a -> Vec a n -> Vec a (E ((n ** Two) - One))
-- vintersperse = undefined
-- vintersperse x (a :> b :> VNil) = a :> x :> b :> VNil
-- vintersperse x (a :> rest) = a :> x :> vintersperse x rest
-- vintersperse x (a :> b :> rest) = case rest of
  -- VNil          -> a :> x :> b :> VNil
  -- (_ :> _ :> _) -> a :> x :> vintersperse x rest

vfoldl :: (b -> a -> b) -> b -> Vec a n -> b
vfoldl _ b VNil      = b
vfoldl f b (x :> xs) = vfoldl f (f b x) xs

vfoldr :: (a -> b -> b) -> b -> Vec a n -> b
vfoldr _ b VNil      = b
vfoldr f b (x :> xs) = f x (vfoldr f b xs)

vconcat :: Vec (Vec a m) n -> Vec a (n ** m)
vconcat VNil = VNil
vconcat (x :> xs) = x +++ vconcat xs

vconcatMap :: (a -> Vec a n) -> Vec a m -> Vec a (m ** n)
vconcatMap _ VNil = VNil
vconcatMap f (x :> xs) = f x +++ vconcatMap f xs
