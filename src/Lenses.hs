{-# LANGUAGE TemplateHaskell #-}
module Lenses where

import Control.Lens hiding (element)


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
