{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
module NatProofsImplicit where

import Data.Type.Equality

data Nat = Z | S Nat

data SNat :: Nat -> * where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

class IsNat (n :: Nat) where nat :: SNat n
instance IsNat Z where nat = SZ
instance IsNat n => IsNat (S n) where nat = SS nat

type family a + b where
  a + Z   = a         -- (1)
  a + S b = S (a + b) -- (2)

type I = S Z
type II = S I

-- Transitive property of propositional equality
(==>) :: a :~: b -> b :~: c -> a :~: c
Refl ==> Refl = Refl

-- Symmetric property of propositional equality
symm :: a :~: b -> b :~: a
symm Refl = Refl

-- Value-level proof of (1)
given1 :: SNat a -> (a + Z) :~: a
given1 _ = Refl

given1' :: IsNat a => (a + Z) :~: a
given1' = Refl

-- Value-level proof of (2)
given2 :: SNat a -> SNat b -> (a + S b) :~: S (a + b)
given2 _ _ = Refl

given2' :: (IsNat a, IsNat b) => (a + S b) :~: S (a + b)
given2' = Refl

-- Right identity
idenR :: SNat a -> (a + Z) :~: a
idenR = given1

-- Left identity
idenL :: SNat a -> (Z + a) :~: a
idenL SZ     = Refl
idenL (SS a) = gcastWith (idenL a) Refl

-- Associativity
assoc :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
assoc a b SZ     = Refl
assoc a b (SS c) = gcastWith (assoc a b c) Refl

-- Commutativity
comm :: SNat a -> SNat b -> (a + b) :~: (b + a)
comm SZ     SZ      = Refl
comm a      SZ      = gcastWith (idenL a) Refl
comm SZ     (SS SZ) = Refl
comm (SS a) (SS SZ) = gcastWith (comm a (SS SZ)) Refl
comm a      k@(SS b)  =
  let
    x1 :: SNat a -> SNat (S b) -> (a + S b) :~: (a + (b + I))
    x1 _ b' = gcastWith (given2 b' (SS SZ)) Refl

    x2 :: SNat a -> SNat b -> (a + (b + I)) :~: ((a + b) + I)
    x2 a' b' = gcastWith (assoc a' b' (SS SZ)) Refl

    x3 :: SNat a -> SNat b -> ((a + b) + I) :~: ((b + a) + I)
    x3 a' b' = gcastWith (comm a' b') Refl

    x4 :: SNat a -> SNat b -> ((b + a) + I) :~: (b + (a + I))
    x4 a' b' = gcastWith (assoc b' a' (SS SZ)) Refl

    x5 :: SNat a -> SNat b -> (b + (a + I)) :~: (b + (I + a))
    x5 a' _ = gcastWith (comm a' (SS SZ)) Refl

    x6 :: SNat a -> SNat b -> (b + (I + a)) :~: ((b + I) + a)
    x6 a' b' = gcastWith (assoc b' (SS SZ) a') Refl

    x7 :: SNat a -> SNat b -> ((b + I) + a) :~: (S b + a)
    x7 _ b' = gcastWith (given2 b' (SS SZ)) Refl

  in x1 a k ==> x2 a b ==> x3 a b ==> x4 a b ==> x5 a b ==> x6 a b ==> x7 a b
