{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module DependentStateMachine where

import Data.Kind

data State = A | B | C | D

type family Transition s where
  Transition A = '[B, C]
  Transition B = '[C]
  Transition C = '[D]
  Transition D = '[]

-- data Elem :: [a] -> a -> Type where
    -- EZ :: Elem (x ': xs) x
    -- ES :: Elem xs x -> Elem (y ': xs) x

type family Elem a as where
  Elem x '[]       = True ~ False
  Elem x (x ': xs) = True ~ True
  Elem x (y ': xs) = Elem x xs

data AState s a = AState a deriving Show

check :: Elem b (Transition a) => AState a n -> AState b n
check = undefined

a :: AState A Int
a = AState 1

b :: AState B Int
b = AState 2

c :: AState C Int
c = AState 3

d :: AState D Int
d = AState 4
