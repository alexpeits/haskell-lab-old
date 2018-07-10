{-# LANGUAGE TemplateHaskell #-}
module Lenses where

import Control.Lens hiding (element)

import System.IO.Unsafe

data Point = Point { _x :: Double, _y :: Double } deriving Show

data Atom = Atom { _element :: String, _point :: Point } deriving Show
data Molecule = Molecule { _atoms :: [Atom] } deriving Show

makeLenses ''Point
makeLenses ''Atom
makeLenses ''Molecule

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

a :: Atom
a = Atom "a" (Point 1 1)

b :: Atom
b = Atom "b" (Point 2 3)

m :: Molecule
m = Molecule [a, b]

data Temp = Temp {_cel :: Int} deriving Show

cel :: Lens' Temp Int
cel f (Temp c) = fmap Temp (f c)

fahr :: Lens' Temp Int
fahr f (Temp c) = fmap (\fa -> Temp (logIt (-) (s1 fa c) fa 1)) (f (logIt (+) (s2 c) c 1))
  where s1 a b = "{in " ++ show a ++ " " ++ show b ++ "}"
        s2 a = "[out " ++ show a ++ "]"

logIt :: (Int -> Int -> Int) -> String -> Int -> Int -> Int
logIt f s a b = unsafePerformIO go
  where go = do
          putStr s
          return $ f a b

t = Temp 1
