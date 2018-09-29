{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
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

-- Plus Associativity
plusAssoc :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
plusAssoc a b SZ     = Refl
plusAssoc a b (SS c) = gcastWith (plusAssoc a b c) Refl

plusAssoc' :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
plusAssoc' a b SZ =
  let proof :: forall x y. SNat x -> SNat y -> ((x + y) + Z) :~: (x + (y + Z))
      proof x y = step1 ==> step2
        where
          step1 :: ((x + y) + Z) :~: (x + y)
          step1  = gcastWith (given1 (x !+ y)) Refl

          step2 :: (x + y) :~: (x + (y + Z))
          step2 = gcastWith (given1 y) Refl
  in proof a b
plusAssoc' a b (SS c) =
  let proof ::
        forall x y z.
        SNat x -> SNat y -> SNat z ->
        ((x + y) + S z) :~: (x + (y + S z))
      proof x y z = step1 ==> step2 ==> step3 ==> step4
        where
          step1 :: ((x + y) + S z) :~: S ((x + y) + z)
          step1 = gcastWith (given2 (x !+ y) (SS z)) Refl

          step2 :: S ((x + y) + z) :~: S (x + (y + z))
          step2 = gcastWith (plusAssoc' x y z) Refl

          step3 :: S (x + (y + z)) :~: (x + S (y + z))
          step3 = gcastWith (given2 x (y !+ z)) Refl

          step4 :: (x + S (y + z)) :~: (x + (y + S z))
          step4 = gcastWith (given2 y z) Refl
  in proof a b c

-- Plus Commutativity
plusComm :: SNat a -> SNat b -> (a + b) :~: (b + a)
plusComm SZ     SZ      = Refl
plusComm a      SZ      = gcastWith (plusIdenL a) Refl
plusComm SZ     (SS SZ) = Refl
plusComm (SS a) (SS SZ) = gcastWith (plusComm a (SS SZ)) Refl
plusComm a      k@(SS b)  =
  let proof :: forall a b. SNat a -> SNat b -> (a + S b) :~: (S b + a)
      proof x y = p1 ==> p2 ==> p3 ==> p4 ==> p5 ==> p6 ==> p7
        where
          p1 :: (a + S b) :~: (a + (b + I))
          p1 = gcastWith (given2 y (SS SZ)) Refl

          p2 :: (a + (b + I)) :~: ((a + b) + I)
          p2 = gcastWith (plusAssoc x y (SS SZ)) Refl

          p3 :: ((a + b) + I) :~: ((b + a) + I)
          p3 = gcastWith (plusComm x y) Refl

          p4 :: ((b + a) + I) :~: (b + (a + I))
          p4 = gcastWith (plusAssoc y x (SS SZ)) Refl

          p5 :: (b + (a + I)) :~: (b + (I + a))
          p5 = gcastWith (plusComm x (SS SZ)) Refl

          p6 :: (b + (I + a)) :~: ((b + I) + a)
          p6 = gcastWith (plusAssoc y (SS SZ) x) Refl

          p7 :: ((b + I) + a) :~: (S b + a)
          p7 = gcastWith (given2 y (SS SZ)) Refl

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
      proof x y z = p1 ==> p2 ==> p3 ==> p4 ==> p5 ==> p6 ==> p7 ==> p8 ==> p9
        where
          p1 :: ((a + b) * S c) :~: (((a + b) * c) + (a + b))
          p1 = Refl -- from (4)

          p2 :: (((a + b) * c) + (a + b)) :~: (((a * c) + (b * c)) + (a + b))
          p2 = gcastWith (mulPlusDist x y z) Refl

          p3 :: (((a * c) + (b * c)) + (a + b)) :~: ((a * c) + ((b * c) + (a + b)))
          p3 = gcastWith (plusAssoc (x !* z) (y !* z) (x !+ y)) Refl

          p4 :: ((a * c) + ((b * c) + (a + b))) :~: ((a * c) + ((a + b) + (b * c)))
          p4 = gcastWith (plusComm (y !* z) (x !+ y)) Refl

          p5 :: ((a * c) + ((a + b) + (b * c))) :~: (((a * c) + (a + b)) + (b * c))
          p5 = gcastWith (plusAssoc (x !* z) (x !+ y) (y !* z)) Refl

          p6 :: (((a * c) + (a + b)) + (b * c)) :~: ((((a * c) + a) + b) + (b * c))
          p6 = gcastWith (plusAssoc (x !* z) x y) Refl

          p7 :: ((((a * c) + a) + b) + (b * c)) :~: (((a * c) + a) + (b + (b * c)))
          p7 = gcastWith (plusAssoc ((x !* z) !+ x) y (y !* z)) Refl

          p8 :: (((a * c) + a) + (b + (b * c))) :~: (((a * c) + a) + ((b * c) + b))
          p8 = gcastWith (plusComm y (y !* z)) Refl

          p9 :: (((a * c) + a) + ((b * c) + b)) :~: ((a * S c) + (b * S c))
          p9 = Refl

  in proof a b c


mulComm :: SNat a -> SNat b -> (a * b) :~: (b * a)
mulComm a SZ       = gcastWith (mulZeroPropL a) Refl
mulComm a k@(SS b) =
  let
    proof :: forall a b c. SNat a -> SNat b -> (a * S b) :~: (S b * a)
    proof x y = p1 ==> p2 ==> p3 ==> p4 ==> p5
      where
        p1 :: (a * S b) :~: ((a * b) + a)
        p1 = Refl

        p2 :: ((a * b) + a) :~: ((b * a) + a)
        p2 = gcastWith (mulComm x y) Refl

        p3 :: ((b * a) + a) :~: ((b * a) + (I * a))
        p3 = gcastWith (mulIdenL x) Refl

        p4 :: ((b * a) + (I * a)) :~: ((b + I) * a)
        p4 = gcastWith (mulPlusDist y (SS SZ) x) Refl

        p5 :: ((b + I) * a) :~: (S b * a)
        p5 = Refl

  in proof a b


mulAssoc :: SNat a -> SNat b -> SNat c -> ((a * b) * c) :~: (a * (b * c))
mulAssoc a b SZ       = Refl
mulAssoc a b k@(SS c) =
  let proof :: forall a b c. SNat a -> SNat b -> SNat c -> ((a * b) * S c) :~: (a * (b * S c))
      proof x y z = p1 ==> p2 ==> p3 ==> p4 ==> p5 ==> p6 ==> p7 ==> p8 ==> p9
        where
          p1 :: ((a * b) * S c) :~: (((a * b) * c) + (a * b))
          p1 = Refl -- from (4)

          p2 :: (((a * b) * c) + (a * b)) :~: ((a * (b * c)) + (a * b))
          p2 = gcastWith (mulAssoc x y z) Refl

          p3 :: ((a * (b * c)) + (a * b)) :~: ((a * b) + (a * (b * c)))
          p3 = gcastWith (plusComm (x !* (y !* z)) (x !* y)) Refl

          p4 :: ((a * b) + (a * (b * c))) :~: ((b * a) + (a * (b * c)))
          p4 = gcastWith (mulComm x y) Refl

          p5 :: ((b * a) + (a * (b * c))) :~: ((b * a) + ((b * c) * a))
          p5 = gcastWith (mulComm x (y !* z)) Refl

          p6 :: ((b * a) + ((b * c) * a)) :~: ((b + (b * c)) * a)
          p6 = gcastWith (mulPlusDist y (y !* z) x) Refl

          p7 :: ((b + (b * c)) * a) :~: (a * (b + (b * c)))
          p7 = gcastWith (mulComm x (y !+ (y !* z))) Refl

          p8 :: (a * (b + (b * c))) :~: (a * ((b * c) + b))
          p8 = gcastWith (plusComm y (y !* z)) Refl

          p9 :: (a * ((b * c) + b)) :~: (a * (b * S c))
          p9 = Refl

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

append' :: SNat n -> SNat m -> Vec n a -> Vec m a -> Vec (n + m) a
append' SZ m V0 ys = gcastWith (plusIdenL m) ys
append' n m (x:>xs) ys = gcastWith (proof pn m) $ x :> append' (spred n) m xs ys
  where
    pn = spred n
    proof :: forall x y. SNat x -> SNat y -> (S x + y) :~: S (x + y)
    proof x y = step1 ==> step2 ==> step3
      where
        step1 :: (S x + y) :~: (y + S x)
        step1 = gcastWith (plusComm (SS x) y) Refl

        step2 :: (y + S x) :~: S (y + x)
        step2 = gcastWith (given2 y (SS x)) Refl

        step3 :: S (y + x) :~: S (x + y)
        step3 = gcastWith (plusComm y x) Refl

-- Implicit version of `append`, thanks to typeclasses
(+++) :: forall n m a. (IsNat n, IsNat m) => Vec n a -> Vec m a -> Vec (n + m) a
(+++) = append (nat @n) (nat @m)

-- get predecessor SNat given a nonzero SNat
spred :: SNat (S n) -> SNat n
spred (SS n) = n

vlength :: IsNat n => Vec n a -> SNat n
vlength _ = nat
