{-# LANGUAGE
    AllowAmbiguousTypes,
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    FlexibleInstances,
    GADTs,
    InstanceSigs,
    MultiParamTypeClasses,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}
module HlistDependent where


data a :* b = a :* b deriving Show

infixr 1 :*

--

hlistExample1 :: Int :* String :* String :* ()
hlistExample1 =  1   :* "One"  :* "1"    :* ()

class Get0 a bs where
  get0 :: bs -> a

instance Get0 a bs => Get0 a (b :* bs) where
  get0 :: (b :* bs) -> a
  get0 (_ :* ys) = (get0 :: bs -> a) ys

instance {-# OVERLAPPING #-} Get0 a (a :* bs) where
  get0 :: (a :* bs) -> a
  get0 (x :* _) = x


getExample1 :: String
getExample1 =
  get0 @String ((1 :: Int) :* "One" :* "1" :* ())

--

class Get1 a bs where
  get1 :: bs -> a

type family a == b :: Bool where
  a == a = 'True
  a == b = 'False

--

type family If (b :: Bool) (c :: k) (d :: k) :: k where
  If 'True c _  = c
  If 'False _ d = d

class IsBool0 b where
  _If0 :: forall r. r -> r -> r

instance IsBool0 'True where
  _If0 x _ = x

instance IsBool0 'False where
  _If0 _ y = y

_If0Examples :: [String]
_If0Examples =
  [ _If0 @'True "This" "That"
    -- "This"

  , _If0 @'False "This" "That"
    -- "That"

  , _If0 @(Int == Bool) "Int is equal to Bool" "Int is not equal to Bool"
    -- "Int is not equal to Bool"
  ]

--

class IsBool b where
  _If
    :: forall r
    .  (('True  ~ b) => r)
    -> (('False ~ b) => r)
    -> r

instance IsBool 'True where
  _If :: forall r. r -> (('False ~ 'True) => r) -> r
  _If x _ = x

instance IsBool 'False where
  -- _If :: forall r. (('True ~ 'False) => r) -> r -> r
  _If _ y = y

class Get a bs where
  get :: bs -> a

type If' b c d = (IsBool b, If b c d)

instance If' (a == b) (a ~ b) (Get a bs)
  => Get a (b :* bs) where
  get :: b :* bs -> a
  get (y :* ys) =
    _If @(a == b)
      y         -- (If 'True (a ~ b) _)     becomes (a ~ b)
      (get ys)  -- (If 'False _ (Get a bs)) becomes (Get a bs)
