{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module NatProofs where

import Data.Type.Equality

data Nat = Z | S Nat

data SNat :: Nat -> * where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

sNatToInt :: SNat n -> Int
sNatToInt SZ     = 0
sNatToInt (SS n) = 1 + sNatToInt n

instance Show (SNat n) where
  show SZ = "0"
  show n  = show $ sNatToInt n

type I = S Z
type II = S I

-- Transitive property of propositional equality
(==>) :: a :~: b -> b :~: c -> a :~: c
Refl ==> Refl = Refl

-- Symmetric property of propositional equality
symm :: a :~: b -> b :~: a
symm Refl = Refl

-- Congruence of propositional equality
cong :: a :~: b -> f a :~: f b
cong Refl = Refl


--
-- Addition
--
type family a + b where
  a + Z   = a         -- (1)
  a + S b = S (a + b) -- (2)

(!+) :: SNat n -> SNat m -> SNat (n + m)
n !+ SZ     = n
n !+ (SS m) = SS (n !+ m)

-- Value-level proof of (1)
given1 :: SNat a -> (a + Z) :~: a
given1 _ = Refl

-- Value-level proof of (2)
given2 :: SNat a -> SNat b -> (a + S b) :~: S (a + b)
given2 _ _ = Refl

-- Right identity
plusIdenR :: SNat a -> (a + Z) :~: a
plusIdenR = given1

-- Left identity
plusIdenL :: SNat a -> (Z + a) :~: a
plusIdenL SZ     = Refl
plusIdenL (SS a) = gcastWith (plusIdenL a) Refl

-- PlusAssociativity
plusAssoc :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
plusAssoc a b SZ     = Refl
plusAssoc a b (SS c) = gcastWith (plusAssoc a b c) Refl

-- PlusCommutativity
plusComm :: SNat a -> SNat b -> (a + b) :~: (b + a)
plusComm SZ     SZ      = Refl
plusComm a      SZ      = gcastWith (plusIdenL a) Refl
plusComm SZ     (SS SZ) = Refl
plusComm (SS a) (SS SZ) = gcastWith (plusComm a (SS SZ)) Refl
plusComm a      k@(SS b)  =
  let proof :: forall a b. SNat a -> SNat b -> (a + S b) :~: (S b + a)
      proof x y = x1 ==> x2 ==> x3 ==> x4 ==> x5 ==> x6 ==> x7
        where
          x1 :: (a + S b) :~: (a + (b + I))
          x1 = gcastWith (given2 y (SS SZ)) Refl

          x2 :: (a + (b + I)) :~: ((a + b) + I)
          x2 = gcastWith (plusAssoc x y (SS SZ)) Refl

          x3 :: ((a + b) + I) :~: ((b + a) + I)
          x3 = gcastWith (plusComm x y) Refl

          x4 :: ((b + a) + I) :~: (b + (a + I))
          x4 = gcastWith (plusAssoc y x (SS SZ)) Refl

          x5 :: (b + (a + I)) :~: (b + (I + a))
          x5 = gcastWith (plusComm x (SS SZ)) Refl

          x6 :: (b + (I + a)) :~: ((b + I) + a)
          x6 = gcastWith (plusAssoc y (SS SZ) x) Refl

          x7 :: ((b + I) + a) :~: (S b + a)
          x7 = gcastWith (given2 y (SS SZ)) Refl

  in proof a b

--
-- Multiplication
--
type family a * b where
  a * Z   = Z           -- (3)
  a * S b = (a * b) + a -- (4)

(!*) :: SNat n -> SNat m -> SNat (n * m)
n !* SZ     = SZ
n !* (SS m) = (n !* m) !+ n

-- Value-level proof of (3)
given3 :: SNat a -> (a * Z) :~: Z
given3 _ = Refl

-- Value-level proof of (4)
given4 :: SNat a -> SNat b -> (a * S b) :~: ((a * b) + a)
given4 _ _ = Refl

-- Right zero property
mulZeroPropR :: SNat a -> (a * Z) :~: Z
mulZeroPropR = given3

-- Left zero property
mulZeroPropL :: SNat a -> (Z * a) :~: Z
mulZeroPropL SZ     = Refl
mulZeroPropL (SS a) = gcastWith (mulZeroPropL a) Refl

-- Right identity
mulIdenR :: SNat a -> (a * I) :~: a
mulIdenR SZ     = Refl
mulIdenR (SS a) = gcastWith (mulIdenR a) Refl

-- Left identity
mulIdenL :: SNat a -> (I * a) :~: a
mulIdenL SZ     = Refl
mulIdenL (SS a) = gcastWith (mulIdenL a) Refl

-- Proof that multiplication distributes over addition
mulPlusDist :: SNat a -> SNat b -> SNat c -> ((a + b) * c) :~: ((a * c) + (b * c))
mulPlusDist a b SZ       = Refl
mulPlusDist a b k@(SS c) =
  let proof :: forall a b c. SNat a -> SNat b -> SNat c -> ((a + b) * S c) :~: ((a * S c) + (b * S c))
      proof x y z = x1 ==> x2 ==> x3 ==> x4 ==> x5 ==> x6 ==> x7 ==> x8 ==> x9
        where
          x1 :: ((a + b) * S c) :~: (((a + b) * c) + (a + b))
          x1 = Refl -- from (4)

          x2 :: (((a + b) * c) + (a + b)) :~: (((a * c) + (b * c)) + (a + b))
          x2 = gcastWith (mulPlusDist x y z) Refl

          x3 :: (((a * c) + (b * c)) + (a + b)) :~: ((a * c) + ((b * c) + (a + b)))
          x3 = gcastWith (plusAssoc xmz ymz xpy) Refl

          x4 :: ((a * c) + ((b * c) + (a + b))) :~: ((a * c) + ((a + b) + (b * c)))
          x4 = gcastWith (plusComm ymz xpy) Refl

          x5 :: ((a * c) + ((a + b) + (b * c))) :~: (((a * c) + (a + b)) + (b * c))
          x5 = gcastWith (plusAssoc xmz xpy ymz) Refl

          x6 :: (((a * c) + (a + b)) + (b * c)) :~: ((((a * c) + a) + b) + (b * c))
          x6 = gcastWith (plusAssoc xmz x y) Refl

          x7 :: ((((a * c) + a) + b) + (b * c)) :~: (((a * c) + a) + (b + (b * c)))
          x7 = gcastWith (plusAssoc xmzpx y ymz) Refl

          x8 :: (((a * c) + a) + (b + (b * c))) :~: (((a * c) + a) + ((b * c) + b))
          x8 = gcastWith (plusComm y ymz) Refl

          x9 :: (((a * c) + a) + ((b * c) + b)) :~: ((a * S c) + (b * S c))
          x9 = Refl

          xmz = x !* z
          ymz = y !* z
          xpy = x !+ y
          xmzpx = xmz !+ x

  in proof a b c


mulComm :: SNat a -> SNat b -> (a * b) :~: (b * a)
mulComm a SZ       = gcastWith (mulZeroPropL a) Refl
mulComm a k@(SS b) =
  let
    proof :: forall a b c. SNat a -> SNat b -> (a * S b) :~: (S b * a)
    proof x y = x1 ==> x2 ==> x3 ==> x4 ==> x5
      where
        x1 :: (a * S b) :~: ((a * b) + a)
        x1 = Refl

        x2 :: ((a * b) + a) :~: ((b * a) + a)
        x2 = gcastWith (mulComm x y) Refl

        x3 :: ((b * a) + a) :~: ((b * a) + (I * a))
        x3 = gcastWith (mulIdenL x) Refl

        x4 :: ((b * a) + (I * a)) :~: ((b + I) * a)
        x4 = gcastWith (mulPlusDist y (SS SZ) x) Refl

        x5 :: ((b + I) * a) :~: (S b * a)
        x5 = Refl

  in proof a b


mulAssoc :: SNat a -> SNat b -> SNat c -> ((a * b) * c) :~: (a * (b * c))
mulAssoc a b SZ       = Refl
mulAssoc a b k@(SS c) =
  let proof :: forall a b c. SNat a -> SNat b -> SNat c -> ((a * b) * S c) :~: (a * (b * S c))
      proof x y z = x1 ==> x2 ==> x3 ==> x4 ==> x5 ==> x6 ==> x7 ==> x8 ==> x9
        where
          x1 :: ((a * b) * S c) :~: (((a * b) * c) + (a * b))
          x1 = Refl -- from (4)

          x2 :: (((a * b) * c) + (a * b)) :~: ((a * (b * c)) + (a * b))
          x2 = gcastWith (mulAssoc x y z) Refl

          x3 :: ((a * (b * c)) + (a * b)) :~: ((a * b) + (a * (b * c)))
          x3 = gcastWith (plusComm xmymz xmy) Refl

          x4 :: ((a * b) + (a * (b * c))) :~: ((b * a) + (a * (b * c)))
          x4 = gcastWith (mulComm x y) Refl

          x5 :: ((b * a) + (a * (b * c))) :~: ((b * a) + ((b * c) * a))
          x5 = gcastWith (mulComm x ymz) Refl

          x6 :: ((b * a) + ((b * c) * a)) :~: ((b + (b * c)) * a)
          x6 = gcastWith (mulPlusDist y ymz x) Refl

          x7 :: ((b + (b * c)) * a) :~: (a * (b + (b * c)))
          x7 = gcastWith (mulComm x ypymz) Refl

          x8 :: (a * (b + (b * c))) :~: (a * ((b * c) + b))
          x8 = gcastWith (plusComm y ymz) Refl

          x9 :: (a * ((b * c) + b)) :~: (a * (b * S c))
          x9 = Refl

          xmymz = x !* (y !* z)
          xmy = x !* y
          ymz = y !* z
          ypymz = y !+ ymz

  in proof a b c

--
-- Use proofs
--
class               IsNat (n :: Nat) where nat :: SNat n
instance            IsNat Z          where nat =  SZ
instance IsNat n => IsNat (S n)      where nat =  SS nat

data Vec :: Nat -> * -> * where
  V0   :: Vec Z a
  (:>) :: a -> Vec n a -> Vec (S n) a

infixr 5 :>

instance (Show a) => Show (Vec n a) where
  show v = "[" ++ go v
    where go :: (Show a') => Vec n' a' -> String
          go v = case v of
            V0        -> "]"
            (x :> xs) -> show x ++ sep ++ go xs
              where sep = case xs of
                      V0   -> ""
                      _    -> ", "

x = 1 :> 2 :> 3 :> 4 :> V0
y = 5 :> 6 :> 7 :> 8 :> 9 :> V0

append :: SNat n -> SNat m -> Vec n a -> Vec m a -> Vec (n + m) a
append SZ m V0 ys = gcastWith (plusIdenL m) ys
append n m (x:>xs) ys = gcastWith (proof n' m) $ x :> append n' m xs ys
  where proof :: SNat n -> SNat m -> (S n + m) :~: S (n + m)
        proof n SZ     = Refl
        proof n (SS m) = gcastWith (proof n m) Refl

        n' = spred n  -- we know that n !~ SZ from the (x:>xs) pattern match


-- Implicit version of `append`, thanks to typeclasses
(+++) :: forall n m a. (IsNat n, IsNat m) => Vec n a -> Vec m a -> Vec (n + m) a
(+++) = append (nat @n) (nat @m)

-- get predecessor SNat given a nonzero SNat
spred :: SNat (S n) -> SNat n
spred (SS n) = n

vlength :: IsNat n => Vec n a -> SNat n
vlength _ = nat
