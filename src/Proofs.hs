{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
module Proofs where

import Data.Type.Equality

data Nat = Zero | Succ Nat

data SNat :: Nat -> * where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

type family a :+ b where
  Zero   :+ b = b
  Succ a :+ b = Succ (a :+ b)

oneTwo :: (Succ Zero :+ Succ Zero) :~: Succ (Succ Zero)
oneTwo = Refl

type family a :& b where
  True :& True = True
  _    :& _    = False

type family a :| b where
  False :| False = False
  _     :| _     = True

type family Not a where
  Not True  = False
  Not False = True

data SBool :: Bool -> * where
  STrue  :: SBool True
  SFalse :: SBool False

deMorgan :: SBool a -> SBool b -> Not (a :& b) :~: (Not a :| Not b)
deMorgan STrue  STrue  = Refl
deMorgan STrue  SFalse = Refl
deMorgan SFalse STrue  = Refl
deMorgan SFalse SFalse = Refl


symmetry :: a :~: b -> b :~: a
symmetry Refl = Refl

transitive :: a :~: b -> b :~: c -> a :~: c
transitive Refl Refl = Refl

cast :: a :~: b -> a -> b
cast Refl x = x

-- n + 0 = n
-- 0 + n = n
plusIdL :: SNat n -> (Zero :+ n) :~: n
plusIdL _ = Refl  -- from the definition of :+
-- plusIdL SZero     = Refl
-- plusIdL (SSucc n) = gcastWith (plusIdL n) Refl

plusIdR :: SNat n -> (n :+ Zero) :~: n
plusIdR SZero     = Refl
plusIdR (SSucc n) = gcastWith (plusIdR n) Refl

plusAssoc :: SNat a -> SNat b -> SNat c -> (a :+ (b :+ c)) :~: ((a :+ b) :+ c)
plusAssoc SZero     b c = Refl
plusAssoc (SSucc a) b c = gcastWith (plusAssoc a b c) Refl

plusComm :: SNat n -> SNat m -> (m :+ n) :~: (n :+ m)
plusComm SZero     SZero = Refl
plusComm SZero     m     = plusIdR m
-- plusComm n         SZero = gcastWith (plusIdR n) Refl
-- plusComm n (SSucc k) = gcastWith (plusComm (SSucc SZero) m) Refl
-- plusComm (SSucc n) m     = let x1 = SSucc
