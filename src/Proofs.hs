{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Proofs where

import Data.Type.Equality

data Nat = Zero | Succ Nat

type family a + b where
  a + Zero   = a
  a + Succ b = Succ (a + b)

oneTwo :: ( Zero + Succ Zero) :~: Succ (Succ Zero)
oneTwo = Refl
