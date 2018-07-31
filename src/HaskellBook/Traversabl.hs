{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module HaskellBook.Traversabl where

import Data.Traversable

newtype Identity a = Identity a deriving (Eq, Ord, Show, Functor, Foldable)

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
