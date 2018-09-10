{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module TypeFamilies where


data family Array a -- compact storage of elements of type a
data instance Array Int = MkArrayInt [Int]
data instance Array String = MkArrayString [String]

testArray :: Array a -> String
testArray x = case x of
  MkArrayInt a -> show a
  MkArrayString a -> head a
