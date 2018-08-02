{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Proxy
import Data.Typeable

import qualified Data.Map as M

import GHC.TypeLits
import GHC.OverloadedLabels

data Foo = Foo (Int, String) deriving Show

instance (KnownSymbol s) => IsLabel s Foo where
  fromLabel = Foo (1, symbolVal (Proxy @s))

type Table = M.Map String String

instance (KnownSymbol s) => IsLabel s (Table -> String) where
  fromLabel m = m M.! symbolVal (Proxy @s)

a = M.fromList [("a", "alfa"), ("b", "beta")]
