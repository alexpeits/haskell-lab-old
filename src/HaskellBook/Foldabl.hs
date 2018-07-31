module HaskellBook.Foldabl where

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f x (Constant b) = f b x

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f x (Two a b) = f b x

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = mappend (f b) (f c)

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF p = foldMap (\x -> if p x then pure x else mempty)
