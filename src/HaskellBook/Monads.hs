module HaskellBook.Monads where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--

doQuickBatch t = do
  quickBatch $ functor t
  quickBatch $ applicative t
  quickBatch $ monad t

type Tester = (Int, String, Double)

data Nope a = Nope deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = Nope

instance Applicative Nope where
  pure _ = Nope
  _ <*> _ = Nope

instance Monad Nope where
  return = pure
  _ >>= _ = Nope

instance Arbitrary (Nope a) where
  arbitrary = return Nope

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

checkNope = do
  let trig :: Nope Tester
      trig = undefined
  doQuickBatch trig

--

data Neither b a = L a | R b deriving (Eq, Show)

instance Functor (Neither b) where
  fmap f (L a) = L (f a)
  fmap _ (R b) = R b

instance Applicative (Neither b) where
  pure = L
  (L f) <*> (L a) = L (f a)
  (R f) <*> _     = R f
  _     <*> (R b) = R b

instance Monad (Neither b) where
  return = pure
  (L a) >>= f = f a
  (R b) >>= _ = R b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Neither b a) where
  arbitrary = frequency [ (1, L <$> arbitrary)
                        , (1, R <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (Neither b a) where
  (=-=) = eq

checkNeither = do
  let trig :: Neither Tester Tester
      trig = undefined
  doQuickBatch trig

--

newtype Identity a = Identity { getId :: a } deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f = Identity . f . getId

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

checkIdentity = do
  let trig :: Identity Tester
      trig = undefined
  doQuickBatch trig

--

data List a = Nil | Cons a (List a) deriving (Eq, Show)

appendList :: List a -> List a -> List a
appendList x Nil = x
appendList Nil x = x
appendList (Cons a as) ass = Cons a (appendList as ass)

concatList :: List (List a) -> List a
concatList Nil = Nil
concatList (Cons a as) = appendList a (concatList as)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil         <*> _           = Nil
  _           <*> Nil         = Nil
  -- (<*>) :: List (a -> b) -> List a -> List b
  fs <*> as = concatList $ fmap (`fmap` as) fs

instance Monad List where
  return = pure
  Nil         >>= _ = Nil
  -- (>>=) :: List a -> (a -> List b) -> List b
  as >>= f = concatList $ fmap f as

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [ (1, return Nil)
                        , (5, Cons <$> arbitrary <*> arbitrary)]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

checkList = do
  let trig :: List Tester
      trig = undefined
  doQuickBatch trig

--

j :: Monad m => m (m a) -> m a
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []     _ = return []
meh (a:as) f = (:) <$> f a <*> meh as f

flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id
