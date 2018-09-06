{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Dependently.Nat where

import Data.Kind

-- Peano natural numbers
data Nat = Zero | Succ Nat | Pred Nat

data Fin :: Nat -> Type where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

type One = 'Succ 'Zero
type Two = 'Succ One
type Three = 'Succ Two

type family n + m where
  Zero   + m = m
  Succ n + m = Succ (n + m)
  Pred n + m = Pred (n + m)

-- | needs UndecidableInstances
type family n ** m where
  Zero   ** m = Zero
  Succ n ** m = m + (n ** m)
  Pred n ** m = (n ** m) - m

type family n - m where
  n      - Zero = n
  Zero   - n    = n ** Pred Zero
  Succ n - m    = Succ (n - m)

type family E n where
  E (Succ (Pred n)) = E n
  E (Pred (Succ n)) = E n
  E (Succ n) = Succ (E n)
  E (Pred n) = Pred (E n)
  E Zero = Zero
  E m = m
