{-# LANGUAGE TemplateHaskell #-}
module Lenses where

import Control.Lens hiding (element)

import Data.Maybe (fromJust)

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

-- other stuff

data User = User {_userName :: String, _userAge :: Int, _userPets :: [Pet], _userPartner :: Maybe User}
data Pet  = Pet  {_petAnimal :: Animal, _petName :: String} deriving Show
data Animal = Dog | Cat deriving Show

instance Show User where
  show (User n a ps p) =
    "User {" ++
    "_userName = " ++ n ++ ", " ++
    "_userAge = " ++ show a ++ ", " ++
    "_userPets = " ++ show ps ++ ", " ++
    "_userPartner = " ++ showPartner p ++ "}"
    where showPartner Nothing = "Nothing"
          showPartner (Just u) = "Just " ++ _userName u

makeLenses ''User
makeLenses ''Pet

u1 = User "Alex" 27 [p1, p2] (Just u2)
u2 = User "Namir" 26 [] (Just u1)
u3 = User "Doe" 30 [] Nothing

p1 = Pet Dog "Azor"
p2 = Pet Cat "Vagelis"

e01 = u1 ^. userName  -- get name of user
e02 = u1 & userPartner . _Just . userName .~ "foo"  -- set name of partner
e03 = u1 ^. userPartner . _Just . userName  -- get name of partner
e04 = u3 ^. userPartner . _Just . userName  -- monoid in action?
e05 = u1 ^. userPets ^? ix 0  -- get first pet
e06 = u1 & userPets . ix 0 . petAnimal .~ Cat  -- set animal of first pet
e07 = u1 & userPets %~ (\xs -> xs ++ xs)  -- clone user's animals
