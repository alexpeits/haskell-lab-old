{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module HaskellBook.Traversabl where

import Data.Traversable

import HaskellBook.Monads

-- newtype Identity a = Identity a deriving (Eq, Ord, Show, Functor, Foldable)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

--

data Big a b = Big a b b deriving (Eq, Show, Functor)

instance Foldable (Big a) where
  foldMap f (Big _ b b') = mappend (f b) (f b')

instance Traversable (Big a) where
  -- traverse :: (b -> f c) -> Big a b -> f (Big a c)
  traverse f (Big a b b') = Big a <$> f b <*> f b'

--

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show, Foldable)

instance Functor (Constant a) where
  -- fmap :: (a -> b) -> Constant x a -> Constant x b
  fmap _ (Constant a) = Constant a

instance Traversable (Constant a) where
  -- traverse :: (a -> f b) -> Constant x a -> f (Constant x b)
  traverse _ (Constant a) = pure $ Constant a

--

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep (f a)

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

--

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons a as) = mappend (f a) $ foldMap f as

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as
