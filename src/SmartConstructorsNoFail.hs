{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module SmartConstructorsNoFail where

import GHC.TypeLits
import Data.Proxy

newtype Refined (ps :: [*]) a = Refined a

refined :: a -> Refined '[] a
refined = Refined

unrefined :: Refined ps a -> a
unrefined (Refined a) = a

class Prop a p where
  type PropProjection a p :: *
  checkProp :: Proxy p -> a -> Either String (PropProjection a p)


data GreaterThan (n :: Nat)

instance (Integral a, KnownNat n) => Prop a (GreaterThan n) where
  type PropProjection a (GreaterThan n) = a -- could be () as well
  checkProp Proxy n =
    if n > fromIntegral (natVal (Proxy :: Proxy n))
      then Right n
      else Left "not your day"
  
