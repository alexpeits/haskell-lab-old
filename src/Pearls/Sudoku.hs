module Pearls.Sudoku where

type Digit    = Char
type Row a    = [a]
type Matrix a = [Row a]
type Grid     = Matrix Digit

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = (== '0')

--
type Choices = [Digit]

solve :: Grid -> [Grid]
solve = filter valid . expand . choices

choices :: Grid -> Matrix Choices
choices = map (map choice)
  where choice d = if blank d then digits else [d]

expand :: Matrix Choices -> [Grid]
expand = cp . map cp
  where cp :: [[a]] -> [[a]]
        cp [] = [[]]
        cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

-- boxs :: Matrix a
