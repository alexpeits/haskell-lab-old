{-# LANGUAGE InstanceSigs #-}
module ComposeTetris where

import Data.Bifunctor

newtype Compose f g a = Compose { unCompose :: f (g a) }


instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ fmap  f <$> fga
  -- fmap f = Compose . fmap (fmap f) . unCompose

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgab) <*> (Compose fga) =
    Compose $ (fmap (<*>) fgab) <*> fga
    -- Compose $ (<*>) <$> fgab <*> fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = foldMap (foldMap f) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  -- sequenceA :: Compose f g (s a) -> s (Compose f g a)

  -- sequenceA :: f (g (s a)) -> s (f (g a))
  -- traverse  :: (a -> s b) -> f (g a) -> s (f (g b))

  -- sequenceA :: g (s a) -> s (g a)
  -- traverse  :: (a -> s b) -> g a -> s (g b)
  traverse :: (Applicative s) => (a -> s b) -> Compose f g a -> s (Compose f g b)
  traverse f = sequenceA . fmap f
  -- traverse f (Compose fga) = Compose <$> traverse (traverse f) fga
