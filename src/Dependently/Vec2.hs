{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module Dependently.Vec2 where

import Data.Kind
import Dependently.Nat

-- Length-indexed vectors
data Vec :: Nat -> Type  -> Type where
  VNil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a

infixr 5 :>

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

class CNat (n :: Nat) where nat :: SNat n
instance CNat Zero where nat = SZero
instance CNat n => CNat (Succ n) where nat = SSucc nat

instance (Show a) => Show (Vec n a) where
  show v = "[" ++ go v
    where go :: (Show a) => Vec n a -> String
          go v = case v of
            VNil      -> "]"
            (x :> xs) -> show x ++ sep ++ go xs
              where sep = case xs of
                      VNil -> ""
                      _    -> ", "

x = 1 :> 2 :> 3 :> 4 :> VNil

instance Functor (Vec n) where
  fmap :: (a -> b) -> Vec n a -> Vec n b
  fmap f VNil      = VNil
  fmap f (x :> xs) = f x :> fmap f xs

vreplicate' :: SNat n -> a -> Vec n a
vreplicate' SZero     _ = VNil
vreplicate' (SSucc n) x = x :> vreplicate' n x

vreplicate :: CNat n => a -> Vec n a
vreplicate = go nat
  where go :: SNat n -> a -> Vec n a
        go SZero     y = VNil
        go (SSucc n) y = y :> go n y


instance CNat n => Applicative (Vec n) where
  pure :: a -> Vec n a
  pure = vreplicate

  (<*>) :: Vec n (a -> b) -> Vec n a -> Vec n b
  (<*>) = undefined
