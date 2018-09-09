{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- -XMultiParamTypeClasses -XFlexibleInstances -XOverloadedLabels -XTypeApplications -XDataKinds -XScopedTypeVariables -XTypeFamilies
module OverloadedLabels where

import Data.Proxy
import Data.String
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
main = do
  print @Int $ #a b
  print @Int $ fromLabel @"a" b
  print $ fromLabel @"b" @(_ -> Int) b

-- | Chris Done's example
-- https://www.reddit.com/r/haskell/comments/4x8tk8/overloadedlabels_considered_awesome/

data Html = Html String [String] [Html] | String String deriving (Show)

instance IsString Html where fromString = String

instance (KnownSymbol symbol, attr ~ String, inner ~ Html) => IsLabel symbol ([attr] -> [inner] -> Html) where
  fromLabel = Html $ symbolVal (Proxy @symbol)

instance (KnownSymbol symbol, attr ~ String, inner ~ Html) => IsLabel symbol ([inner] -> Html) where
  fromLabel = Html (symbolVal (Proxy @symbol)) []

document :: Html
document = #html [#body [#p ["foo=bar", "baz=grok"] ["Hello!"]]]
