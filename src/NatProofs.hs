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
  let
    x1 :: SNat a -> SNat (S b) -> (a + S b) :~: (a + (b + I))
    x1 _ b' = gcastWith (given2 b' (SS SZ)) Refl

    x2 :: SNat a -> SNat b -> (a + (b + I)) :~: ((a + b) + I)
    x2 a' b' = gcastWith (plusAssoc a' b' (SS SZ)) Refl

    x3 :: SNat a -> SNat b -> ((a + b) + I) :~: ((b + a) + I)
    x3 a' b' = gcastWith (plusComm a' b') Refl

    x4 :: SNat a -> SNat b -> ((b + a) + I) :~: (b + (a + I))
    x4 a' b' = gcastWith (plusAssoc b' a' (SS SZ)) Refl

    x5 :: SNat a -> SNat b -> (b + (a + I)) :~: (b + (I + a))
    x5 a' _ = gcastWith (plusComm a' (SS SZ)) Refl

    x6 :: SNat a -> SNat b -> (b + (I + a)) :~: ((b + I) + a)
    x6 a' b' = gcastWith (plusAssoc b' (SS SZ) a') Refl

    x7 :: SNat a -> SNat b -> ((b + I) + a) :~: (S b + a)
    x7 _ b' = gcastWith (given2 b' (SS SZ)) Refl

  in x1 a k ==> x2 a b ==> x3 a b ==> x4 a b ==> x5 a b ==> x6 a b ==> x7 a b


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

mulPlusDistHelper ::
  forall a b c.
  (
    IsNat a, IsNat b, IsNat c
  , IsNat (a * c), IsNat (b * c), IsNat (a + b)
  , IsNat ((a * c) + a)
  ) =>
  SNat a -> SNat b -> SNat c ->
  (((a + b) * c) + (a + b)) :~: ((a * S c) + (b * S c))
mulPlusDistHelper a b c =
  let
    x1 :: (((a + b) * c) + (a + b)) :~: (((a * c) + (b * c)) + (a + b))
    x1 = gcastWith (mulPlusDist (nat @a) (nat @b) (nat @c)) Refl

    x2 :: (((a * c) + (b * c)) + (a + b)) :~: ((a * c) + ((b * c) + (a + b)))
    x2 = gcastWith (plusAssoc (nat @(a * c)) (nat @(b * c)) (nat @(a + b))) Refl

    x3 :: ((a * c) + ((b * c) + (a + b))) :~: ((a * c) + ((a + b) + (b * c)))
    x3 = gcastWith (plusComm (nat @(b * c)) (nat @(a + b))) Refl

    x4 :: ((a * c) + ((a + b) + (b * c))) :~: (((a * c) + (a + b)) + (b * c))
    x4 = gcastWith (plusAssoc (nat @(a * c)) (nat @(a + b)) (nat @(b * c))) Refl

    x5 :: (((a * c) + (a + b)) + (b * c)) :~: ((((a * c) + a) + b) + (b * c))
    x5 = gcastWith (plusAssoc (nat @(a * c)) (nat @a) (nat @b)) Refl

    x6 :: ((((a * c) + a) + b) + (b * c)) :~: (((a * c) + a) + (b + (b * c)))
    x6 = gcastWith (plusAssoc (nat @((a * c) + a)) (nat @b) (nat @(b * c))) Refl

    x7 :: (((a * c) + a) + (b + (b * c))) :~: (((a * c) + a) + ((b * c) + b))
    x7 = gcastWith (plusComm (nat @b) (nat @(b * c))) Refl

    x8 :: (((a * c) + a) + ((b * c) + b)) :~: ((a * S c) + (b * S c))
    x8 = Refl

  in x1 ==> x2 ==> x3 ==> x4 ==> x5 ==> x6 ==> x7 ==> x8

-- Proof that multiplication distributes over addition
mulPlusDist :: SNat a -> SNat b -> SNat c -> ((a + b) * c) :~: ((a * c) + (b * c))
mulPlusDist a b SZ       = Refl
mulPlusDist a b k@(SS c) =
  let
    x1 :: SNat a -> SNat b -> SNat (S c) -> ((a + b) * S c) :~: (((a + b) * c) + (a + b))
    x1 _ _ _ = Refl -- from (4)

    x2 :: SNat a -> SNat b -> SNat c -> (((a + b) * c) + (a + b)) :~: (((a * c) + (b * c)) + (a + b))
    x2 a' b' c' = gcastWith (mulPlusDist a' b' c') Refl

    x3 ::
      SNat a -> SNat b -> SNat c ->
      SNat (a * c) -> SNat (b * c) -> SNat (a + b) ->
      (((a * c) + (b * c)) + (a + b)) :~: ((a * c) + ((b * c) + (a + b)))
    x3 _ _ _ x y z = gcastWith (plusAssoc x y z) Refl

    x4 ::
      SNat a -> SNat b -> SNat c ->
      SNat (b * c) -> SNat (a + b) ->
      ((a * c) + ((b * c) + (a + b))) :~: ((a * c) + ((a + b) + (b * c)))
    x4 _ _ _ x y = gcastWith (plusComm x y) Refl

    x5 ::
      SNat a -> SNat b -> SNat c ->
      SNat (a * c) -> SNat (a + b) -> SNat (b * c) ->
      ((a * c) + ((a + b) + (b * c))) :~: (((a * c) + (a + b)) + (b * c))
    x5 _ _ _ x y z = gcastWith (plusAssoc x y z) Refl

    x6 ::
      SNat a -> SNat b -> SNat c ->
      SNat (a * c) ->
      (((a * c) + (a + b)) + (b * c)) :~: ((((a * c) + a) + b) + (b * c))
    x6 a' b' _ x = gcastWith (plusAssoc x a' b') Refl

    x7 ::
      SNat a -> SNat b -> SNat c ->
      SNat ((a * c) + a) -> SNat (b * c) ->
      ((((a * c) + a) + b) + (b * c)) :~: (((a * c) + a) + (b + (b * c)))
    x7 _ b' _ x y = gcastWith (plusAssoc x b' y) Refl

    x8 ::
      SNat a -> SNat b -> SNat c ->
      SNat (b * c) ->
      (((a * c) + a) + (b + (b * c))) :~: (((a * c) + a) + ((b * c) + b))
    x8 _ b' _ x = gcastWith (plusComm b' x) Refl

    x9 ::
      SNat a -> SNat b -> SNat c ->
      (((a * c) + a) + ((b * c) + b)) :~: ((a * S c) + (b * S c))
    x9 _ _ _ = Refl

    amc = a !* c
    bmc = b !* c
    apb = a !+ b
    amcpa = amc !+ a

  in x1 a b k ==> x2 a b c ==> x3 a b c amc bmc apb ==> x4 a b c bmc apb ==> x5 a b c amc apb bmc ==> x6 a b c amc ==> x7 a b c amcpa bmc ==> x8 a b c bmc ==> x9 a b c

-- Associativity
mulAssoc :: SNat a -> SNat b -> SNat c -> ((a * b) * c) :~: (a * (b * c))
mulAssoc a b SZ       = Refl
mulAssoc a b k@(SS c) =
  let
    x1 :: SNat a -> SNat b -> SNat (S c) -> ((a * b) * S c) :~: (((a * b) * c) + (a * b))
    x1 _ _ _ = Refl -- from (4)

    x2 :: SNat a -> SNat b -> SNat c -> (((a * b) * c) + (a * b)) :~: ((a * (b * c)) + (a * b))
    x2 a' b' c' = gcastWith (mulAssoc a' b' c') Refl

    x3 :: SNat a -> SNat b -> SNat c ->
          SNat (a * (b * c)) -> SNat (a * b) ->
          ((a * (b * c)) + (a * b)) :~: ((a * b) + (a * (b * c)))
    x3 _ _ _ x y = gcastWith (plusComm x y) Refl

    x4 ::
      SNat a -> SNat b -> SNat c ->
      ((a * b) + (a * (b * c))) :~: ((b * a) + (a * (b * c)))
    x4 a' b' _ = gcastWith (mulComm a' b') Refl

    x5 ::
      SNat a -> SNat b -> SNat c ->
      SNat (b * c) ->
      ((b * a) + (a * (b * c))) :~: ((b * a) + ((b * c) * a))
    x5 a' _ _ x = gcastWith (mulComm a' x) Refl

    x6 ::
      SNat a -> SNat b -> SNat c ->
      SNat (b * c) ->
      ((b * a) + ((b * c) * a)) :~: ((b + (b * c)) * a)
    x6 a' b' _ x = gcastWith (mulPlusDist b' x a') Refl

    x7 ::
      SNat a -> SNat b -> SNat c ->
      SNat (b + (b * c)) ->
      ((b + (b * c)) * a) :~: (a * (b + (b * c)))
    x7 a' _ _ x = gcastWith (mulComm a' x) Refl

    x8 ::
      SNat a -> SNat b -> SNat c ->
      SNat (b * c) ->
      (a * (b + (b * c))) :~: (a * ((b * c) + b))
    x8 _ b' _ x = gcastWith (plusComm b' x) Refl

    x9 ::
      SNat a -> SNat b -> SNat c ->
      (a * ((b * c) + b)) :~: (a * (b * S c))
    x9 _ _ _ = Refl

    ambmc = a !* (b !* c)
    amb = a !* b
    bmc = b !* c
    bpbmc = b !+ bmc

  in x1 a b k ==> x2 a b c ==> x3 a b c ambmc amb ==> x4 a b c ==> x5 a b c bmc ==> x6 a b c bmc ==> x7 a b c bpbmc ==> x8 a b c bmc ==> x9 a b c

mulComm :: SNat a -> SNat b -> (a * b) :~: (b * a)
mulComm a SZ       = gcastWith (mulZeroPropL a) Refl
mulComm a k@(SS b) =
  let
    x1 :: SNat a -> SNat (S b) -> (a * S b) :~: ((a * b) + a)
    x1 _ _ = Refl

    x2 :: SNat a -> SNat b -> ((a * b) + a) :~: ((b * a) + a)
    x2 a' b' = gcastWith (mulComm a' b') Refl

    x3 :: SNat a -> SNat b -> ((b * a) + a) :~: ((b * a) + (I * a))
    x3 a' _ = gcastWith (mulIdenL a') Refl

    x4 :: SNat a -> SNat b -> ((b * a) + (I * a)) :~: ((b + I) * a)
    x4 a' b' = gcastWith (mulPlusDist b' (SS SZ) a') Refl

    x5 :: SNat a -> SNat b -> ((b + I) * a) :~: (S b * a)
    x5 _ b' = Refl

  in x1 a k ==> x2 a b ==> x3 a b ==> x4 a b ==> x5 a b

mulAssoc' :: SNat a -> SNat b -> SNat c -> ((a * b) * c) :~: (a * (b * c))
mulAssoc' a b SZ       = Refl
mulAssoc' a b k@(SS c) =
  let proof :: forall a b c. SNat a -> SNat b -> SNat c -> ((a * b) * S c) :~: (a * (b * S c))
      proof x y z = x1 ==>
                    x2 ==>
                    x3 ==>
                    x4 ==>
                    x5 ==>
                    x6 ==>
                    x7 ==>
                    x8 ==>
                    x9
        where
          x1 :: ((a * b) * S c) :~: (((a * b) * c) + (a * b))
          x1 = Refl -- from (4)

          x2 :: (((a * b) * c) + (a * b)) :~: ((a * (b * c)) + (a * b))
          x2 = gcastWith (mulAssoc' x y z) Refl

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

-- x = 1 :> 2 :> 3 :> 4 :> V0
-- y = 5 :> 6 :> 7 :> 8 :> 9 :> V0

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
