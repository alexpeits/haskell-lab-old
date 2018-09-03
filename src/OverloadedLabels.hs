{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- -XMultiParamTypeClasses -XFlexibleInstances -XOverloadedLabels -XTypeApplications -XDataKinds -XScopedTypeVariables -XTypeFamilies
module OverloadedLabels where

import Data.Proxy
-- import Data.Typeable

import qualified Data.Map as M

import GHC.TypeLits
import GHC.OverloadedLabels

data Foo = Foo (Int, String) deriving Show

-- silly example
instance (KnownSymbol s) => IsLabel s Foo where
  fromLabel = Foo (1, symbolVal (Proxy @s))

data Bar = L Int | R String deriving Show

instance (KnownSymbol s) => IsLabel s Bar where
  fromLabel = R $ symbolVal (Proxy @s)

instance (KnownSymbol s) => IsLabel s (Bar -> String) where
  fromLabel (R x) = x ++ symbolVal (Proxy @s)

type StringMap = M.Map String String

a :: StringMap
a = M.fromList [("a", "alpha"), ("b", "beta")]

instance (KnownSymbol s, x ~ String) => IsLabel s (StringMap -> x) where
  fromLabel m = m M.! symbolVal (Proxy @s)

type IntMap = M.Map String Int

b :: IntMap
b = M.fromList [("a", 1), ("b", 2)]

instance (KnownSymbol s) => IsLabel s (IntMap -> Int) where
  fromLabel m = m M.! symbolVal (Proxy @s)

main :: IO ()
main = -- print @Int $ #a b
  print $ fromLabel @"b" @(_ -> Int) b
