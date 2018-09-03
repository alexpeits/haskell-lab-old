{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module HKD where

import Data.Functor.Identity

import GHC.Generics

type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data Person' f = Person
  { pName :: HKD f String
  , pAge  :: HKD f Int
  } deriving (Generic)

-- | asdf
type Person = Person' Identity

instance Show (Person' Identity) where
  show (Person n a) = "Person " ++ show n ++ " " ++ show a

a :: Person
a = Person "a" 2
