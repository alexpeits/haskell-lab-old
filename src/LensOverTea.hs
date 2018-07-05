{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module LensOverTea where


import Control.Applicative
import Control.Arrow ((&&&))

import Data.Functor.Identity
import Data.Functor.Const

import Data.Monoid (First(..), Any(..), Endo(..))

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a


-- 1

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 afb (a, x) = fmap (, x) (afb a)

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 afb (x, a) = fmap (x, ) (afb a)

-- Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set afb s = set s <$> afb (get s)

-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
-- Lens s1 t1 a b = (a -> f b) -> s1 -> f t1
-- Lens s2 t2 a b = (a -> f b) -> s2 -> f t2
-- Lens (Either s1 s2) (Either t1 t2) a b = (a -> f b) -> Either s1 s2 -> f (Either t1 t2)
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 afb es = case es of
  Left s1  -> Left <$> l1 afb s1
  Right s2 -> Right <$> l2 afb s2

-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)
-- | Lens s t a b = (a -> (b, ) b) -> s -> (b, ) t
--   (b, ) is the Functor
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l ab = l (ab &&& ab)

-- Modify the target of a lens, but return the old value.
-- | Lens s t a b = (a -> (a, ) b) -> s -> (a, ) t
--   (a, ) is the Functor
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l ab = l (id &&& ab)

-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
united :: Lens' s ()
-- united afa s = fmap (const s) (afa ())
united = lens (const ()) const


_abs :: Real a => Lens' a a
_abs f n = update <$> f (abs n)
  where
    update x
      | x < 0     = error "_abs: absolute value can't be negative"
      | otherwise = signum n * x

_all :: Eq a => a -> Lens' [a] a
_all ref = lens get set
  where
    get s     = ref
    set s new = map (\old -> if old == ref then new else old) s

_all' :: Eq a => a -> Traversal' [a] a
_all' ref f = traverse update
  where
    update old = if old == ref then f old else pure old

-- type AppLens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
-- type AppLens' s a = AppLens s s a a
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

-- type Getting s a = (a -> Const a a) -> s -> Const a s
-- Getting r s a is a function which, given some way to get r from a, will go
-- over as in some s and return their combined rs.
type Getting r s a = (a -> Const r a) -> s -> Const r s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

-- over :: Lens s t a b -> (a -> b) -> s -> t
over :: Setting s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

-- set :: Lens s t a b -> b -> s -> t
set :: Setting s t a b -> b -> s -> t
set l f = runIdentity . l (Identity . const f)

-- view :: Lens' s a -> (s -> a)
view :: Getting a s a -> s -> a
view l = getConst . l Const

-- toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
-- toListOf :: Getting [a] s a -> s -> [a]
-- toListOf :: Getting (AppendList a) s a -> s -> [a]
toListOf :: Getting (Endo [a]) s a -> s -> [a]
-- toListOf l = getConst . l (\x -> Const [x])
-- toListOf l = doAppends . getConst . l (\x -> Const (JustList [x]))
toListOf l = (`appEndo` []) . getConst . l (\x -> Const (Endo (x:)))
-- preview
  -- :: ((a -> Const (First a) a) -> s -> Const (First a) s)
  -- -> s
  -- -> Maybe a
preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst . getConst . l (Const . First . Just)

-- has :: ((a -> Const Any a) -> s -> Const Any s) -> s -> Bool
has :: Getting Any s a -> s -> Bool
has l = getAny . getConst . l (\_ -> Const (Any True))

data AppendList a = JustList [a]
                  | Append (AppendList a) (AppendList a)
                  deriving Show

doAppends :: AppendList a -> [a]
doAppends (JustList x) = x
doAppends (Append (JustList x) y) = x ++ doAppends y
doAppends (Append (Append a b) y) = doAppends (Append a (Append b y))

instance Monoid (AppendList a) where
  mempty = JustList []
  mappend = Append

class Each s t a b where -- | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b

instance Traversable t => Each (t a) (t b) a b where
  each = traverse

-- instance (a ~ a', b ~ b') => Each (a, a') (b, b') a b where
--   each f ~(a,b) = (,) <$> f a <*> f b
