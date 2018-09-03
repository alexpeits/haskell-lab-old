{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module StitchDependent where

import Data.Kind

-- Peano natural numbers
data Nat = Zero | Succ Nat

type One = 'Succ 'Zero
type Two = 'Succ One
type Three = 'Succ Two

-- Length-indexed vectors
data Vec :: Type -> Nat -> Type where
  VNil :: Vec a Zero
  (:>) :: a -> Vec a n -> Vec a (Succ n)

infixr 5 :>

-- | needs StandaloneDeriving
-- deriving instance Show a => Show (Vec a n)

instance (Show a) => Show (Vec a n) where
  show v = "[" ++ go v
    where go :: (Show a) => Vec a n -> String
          go v = case v of
            VNil      -> "]"
            (x :> xs) -> show x ++ sep ++ go xs
              where sep = case xs of
                      VNil -> ""
                      _    -> ", "

vecSum :: Vec Int n -> Int
vecSum VNil = 0
vecSum (x :> xs) = x + vecSum xs

vnil = VNil :: Vec Int Zero
x = 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> VNil
y = 7 :> 8 :> 9 :> 10 :> VNil

-- Appending
type family n + m where
  Zero   + m = m
  Succ n + m = Succ (n + m)

(+++) :: Vec a n -> Vec a m -> Vec a (n + m)
VNil      +++ ys = ys
(x :> xs) +++ ys = x :> (xs +++ ys)

--- Indexing
data Fin :: Nat -> Type where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

(!!!) :: Vec a n -> Fin n -> a
vec !!! fin = case (fin, vec) of
  (FZ, x :> _)    -> x
  (FS n, _ :> xs) -> xs !!! n

-- Existentials
data Ex :: (k -> Type) -> Type where
  Ex :: a i -> Ex a

exVecSum :: Ex (Vec Int) -> Int
exVecSum (Ex v) = go v
  where go :: Vec Int n -> Int
        go VNil      = 0
        go (x :> xs) = x + go xs

type Matrix = Vec (Ex (Vec Int))

sumMatrix :: Matrix n -> Int
sumMatrix VNil = 0
sumMatrix (x :> xs) = exVecSum x + sumMatrix xs

-- Singletons
data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

replicateVec :: SNat n -> a -> Vec a n
replicateVec SZero      _   = VNil
replicateVec (SSucc n') elt = elt :> replicateVec n' elt

class SNatl (n :: Nat) where snat :: SNat n
instance SNatl Zero where snat = SZero
instance SNatl n => SNatl (Succ n) where snat = SSucc snat

replicateVec' :: (SNatl n) => a -> Vec a n
replicateVec' = replicateVec snat

r = replicateVec' @Three 1

-- Verious operations
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

-- | needs UndecidableInstances
type family n ** m where
  Zero   ** m = Zero
  Succ n ** m = m + (n ** m)

type family n - m where
  n      - Zero = n
  Succ n - m    = Succ (n - m)

vintersperse :: a -> Vec a (Succ (Succ n)) -> Vec a ((Two ** (Succ (Succ n))) - One)
