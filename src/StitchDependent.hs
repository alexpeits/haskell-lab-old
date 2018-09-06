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

-- Length-indexed vectors
data Vec :: Type -> Nat -> Type where
  VNil :: Vec a Zero
  (:>) :: a -> Vec a n -> Vec a (Succ n)

infixr 5 :>

-- | needs StandaloneDeriving
-- deriving instance Show a => Show (Vec a n)

vecSum :: Vec Int n -> Int
vecSum VNil = 0
vecSum (x :> xs) = x + vecSum xs

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
