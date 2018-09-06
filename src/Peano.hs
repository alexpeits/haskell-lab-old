{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Peano where

data Nat = Zero | Succ Nat | Pred Nat

type family n + m where
  Zero   + m = E m
  Succ n + m = Succ (E n + E m)
  Pred n + m = Pred (E n + E m)

type family E n where
  E (Succ (Pred n)) = E n
  E (Pred (Succ n)) = E n
  E (Succ n) = Succ (E n)
  E (Pred n) = Pred (E n)
  E Zero = Zero

data Vec :: (* -> Nat -> *) where
  VNil :: Vec a Zero
  (:>) :: a -> Vec a n -> Vec a (Succ n)

(+++) :: Vec a n -> Vec a m -> Vec a (n + m)
VNil      +++ ys = ys
(x :> xs) +++ ys = x :> (xs +++ ys)

