module HaskellBook.Applicatives where

-- []
-- pure :: a -> [a]
-- (<*>) :: [a -> b] -> [a] -> [b]

-- (,) a
-- pure :: a -> (mempty, a)
-- (<*>) :: f (a -> b) -> f a -> f b
-- f          :: (,) x
-- f (a -> b) :: (x, a -> b)
-- (<*>) :: Monoid m => (m, a -> b) -> (m, a) -> (m, b)

-- (->) e
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
