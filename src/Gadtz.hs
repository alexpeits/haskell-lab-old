{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Gadtz where

-- this is partial - can lead to runtime error
data A = A1 { aField1 :: Int }
       | A2 { aField2 :: Int }
       deriving Show

-- Runtime error!
a :: Int
a = aField1 $ A2 1


-- Let's try with GADTs

data B where
  B1 :: { bField1 :: Int } -> B
  B2 :: { bField2 :: Int } -> B

b :: Int
b = bField1 $ B2 1

-- not quite, but:

data C1Ty
data C2Ty

data C a where
  C1 :: { cField1 :: Int } -> C C1Ty
  C2 :: { cField2 :: Int } -> C C2Ty

-- this won't compile now
-- c :: Int
-- c = cField1 $ C2 1

-- but this will
c' :: Int
c' = cField2 $ C2 1
-- because cField2 :: C C2Ty -> Int


-- let's see if DataKinds offer anything

data DTy = D1Ty | D2Ty

data D a where
  D1 :: { dField1 :: Int } -> D 'D1Ty
  D2 :: { dField2 :: Int } -> D 'D2Ty

d :: Int
d = dField2 $ D2 4
-- meh

-- d' :: D a -> a
-- d' D1 _ = D1Ty
-- d' D2 _ = D2Ty
