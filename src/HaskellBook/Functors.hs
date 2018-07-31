module HaskellBook.Functors where

import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = do
  putStrLn "Running specs"
  hspec spec
  putStrLn "\nRunning prop tests\n"
  where spec :: Spec
        spec = describe "Monoid, Semigroup" $ do
          undefined

data Three a b = Three a b b deriving Show

instance Functor (Three a) where
  -- fmap :: (a -> b) -> Three _ a -> Three _ b
  fmap f (Three a b c) = Three a (f b) (f c)

data BoolAndSomethingElse a = False' a | True' a deriving Show

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

newtype Mu f = InF { outF :: f (Mu f) }
-- no functor

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  -- fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa
