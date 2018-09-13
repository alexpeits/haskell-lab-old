{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module DependentStateMachine where

import Data.Kind

data AState = A | B | C | D

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

data State :: AState -> * -> * where
  State :: s -> State t s

check :: Elem m (Transition n) => State n a -> State m a
check = undefined

switch :: State A Int -> IO (State B Int)
switch (State x) = putStrLn "yea" >> return (State (x + 1))

switchIllegal :: State A Int -> IO (State D Int)
switchIllegal (State x) = putStrLn "yea" >> return (State (x + 1))

proceed :: Elem m (Transition n) => State n a -> (State n a -> IO (State m a)) -> IO (State m a)
proceed s f = f s

a :: State A Int
a = State 1

b :: State B Int
b = State 2

c :: State C Int
c = State 3

d :: State D Int
d = State 4
