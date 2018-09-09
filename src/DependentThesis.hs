{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
module DependentThesis where

import Data.Kind

data Nat = Zero | Succ Nat -- first, some natural numbers

data Vec :: Type -> Nat -> Type where
  VNil  :: Vec a 'Zero
  (:>) :: a -> Vec a n -> Vec a ('Succ n)

infixr 5 :>

type family n +! m where
  Zero   +! m = m
  Succ n +! m = Succ (n +! m)

vappend :: Vec a n -> Vec a m -> Vec a (n +! m)
vappend VNil      ys = ys
vappend (x :> xs) ys = x :> vappend xs ys

data Ty = Unit | Ty :~> Ty
infixr 0 :~>

data Elem :: [a] -> a -> Type where
  EZ ::              Elem (x ': xs) x
  ES :: Elem xs x -> Elem (y ': xs) x

data Expr :: [Ty] -> Ty -> Type where
  Var :: Elem ctx ty                             -> Expr ctx ty
  Lam :: Expr (arg ': ctx) res                   -> Expr ctx (arg ':~> res)
  App :: Expr ctx (arg ':~> res) -> Expr ctx arg -> Expr ctx res
  TT  ::                                            Expr ctx 'Unit

data Val :: Ty -> Type where
  LamVal :: Expr '[arg] res -> Val (arg ':~> res)
  TTVal  ::                    Val 'Unit

eval :: Expr '[] ty -> Val ty
eval = undefined

shift :: forall ctx ty x. Expr ctx ty -> Expr (x ': ctx) ty -> Expr ctx ty
shift = undefined

subst :: forall ctx s ty. Expr ctx s -> Expr (s ': ctx) ty -> Expr ctx ty
subst = undefined

apply :: Val (arg ':~> res) -> Expr '[] arg -> Expr '[] res
apply = undefined
