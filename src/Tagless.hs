{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Tagless where


class ExpAlg t where
  lit :: Int -> t
  add :: t -> t -> t
  mul :: t -> t -> t


e1 :: ExpAlg a => a
e1 =
  add (lit 1)
      (add (lit 2)
           (lit 3))

e2 :: ExpAlg a => a
e2 =
  add (lit 1)
      (mul (lit 2)
           (lit 3))


showBin :: String -> String -> String -> String
showBin op x y = "(" ++ x ++ " " ++ op ++ " " ++ y ++ ")"

instance ExpAlg Int where
  lit = id
  add = (+)
  mul = (*)

instance ExpAlg String where
  lit = show
  add = showBin "+"
  mul = showBin "*"
