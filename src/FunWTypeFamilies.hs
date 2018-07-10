{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module FunWTypeFamilies where

import Data.Proxy
import Data.Functor.Identity

data Zero
data Succ n

type Two   = Succ (Succ Zero)
type Three = Succ (Succ (Succ Zero))

data Tru
data Fals

instance Show Tru where
  show _ = "Tru"
instance Show Fals where
  show _ = "Fals"

class Even n where
  type IsEven n :: *

instance Even Zero where
  type IsEven Zero = Tru

class Odd n where
  type IsOdd n :: *

-- instance Even Zero Tru
-- instance Odd n b => Even (Succ n) b
-- instance Odd Zero Fals
-- instance Even n b => Odd (Succ n) b

-- class Add a b c | a b -> c where add :: a -> b -> c
-- instance Add Zero b b
-- instance Add a b c => Add (Succ a) b (Succ c)

-- class Mul a b c | a b -> c where mul :: a -> b -> c
-- instance Mul Zero b Zero
-- instance (Mul a b c, Add b c d) => Mul (Succ a) b d
