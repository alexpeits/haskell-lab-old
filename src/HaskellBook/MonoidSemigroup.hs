{-# LANGUAGE DeriveGeneric #-}
module HaskellBook.MonoidSemigroup where

import Data.Monoid
import qualified Data.Semigroup as S

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Function

import GHC.Generics

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid  a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only $ mappend a b
  mappend Nada Nada = Nada
  mappend a Nada = a
  mappend Nada a = a

testOptionalMonoid = it "Optional monoid" $ do
  Only (Sum 1) `mappend` Only (Sum 1) `shouldBe` Only (Sum 2)
  Only (Product 4) `mappend` Only (Product 2) `shouldBe` Only Product {getProduct = 8}
  Only (Sum 1) `mappend` Nada `shouldBe` Only Sum {getSum = 1}
  Only [1] `mappend` Nada `shouldBe` Only [1]
  Nada `mappend` Only (Sum 1) `shouldBe` Only Sum {getSum = 1}

monoidAssocLaw :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssocLaw a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdLaw :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdLaw a = (a <> mempty) == a

monoidRightIdLaw :: (Eq m, Monoid m) => m -> Bool
monoidRightIdLaw a = a == (a <> mempty)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only a)) _ = First' (Only a)
  mappend _ x = x

testFirstMonoid = it "First' monoid" $ do
  First' (Only 1) `mappend` First' Nada `shouldBe` First' (Only 1)
  First' Nada `mappend` First' Nada `shouldBe` First' (Nada :: Optional Int)
  First' Nada `mappend` First' (Only 2) `shouldBe` First' (Only 2)
  First' (Only 1) `mappend` First' (Only 2) `shouldBe` First' (Only 1)

main :: IO ()
main = do
  putStrLn "Running specs"
  hspec spec
  putStrLn "\nRunning prop tests\n"
  quickCheck (monoidAssocLaw :: String -> String -> String -> Bool)
  quickCheck (monoidLeftIdLaw :: String -> Bool)
  quickCheck (monoidRightIdLaw :: String -> Bool)
  where spec :: Spec
        spec = describe "Monoid, Semigroup" $ do
            testOptionalMonoid
            testFirstMonoid

--

data Trivial = Trivial deriving (Eq, Show)

instance S.Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance EqProp Trivial where
  (=-=) = eq

testTrivial = quickBatch $ semigroup Trivial

--

newtype Identity a = Identity a deriving (Eq, Show)

instance S.Semigroup a => S.Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity $ a S.<> b

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

testIdentity = do
  let trig :: Identity (Sum Int)
      trig = undefined
  quickBatch $ semigroup trig

--

data Two a b = Two a b deriving (Eq, Show)

instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a S.<> c) (b S.<> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

testTwo = do
  let trig :: Two (Sum Int) (Product Int)
      trig = undefined
  quickBatch $ semigroup trig

--

newtype Combine a b = Combine { unCombine :: a -> b } deriving Generic

instance (S.Semigroup b) => S.Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> f x S.<> g x

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

-- testCombine = quickCheck (testCombine' :: Cmb -> Cmb -> Cmb -> Bool)
--   where testCombine' :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool
--         testCombine' a b c =
--           (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

--

newtype Comp a = Comp (a -> a)

instance (S.Semigroup a) => S.Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

--

data Validation a b = Fail a | Suc b deriving (Eq, Show)

instance S.Semigroup a => S.Semigroup (Validation a b) where
  (Fail x) <> (Fail y) = Fail $ x S.<> y
  (Suc x) <> _ = Suc x
  _ <> (Suc y) = Suc y

instance (Monoid a) => Monoid (Validation a b) where
  mempty = Fail mempty
  mappend = (<>)

--

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance (S.Semigroup a) => S.Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem $ \x ->
    let (a, s) = f x
        (a', s') = g s
    in (a S.<> a', s')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)
  mappend = (<>)
