{-# LANGUAGE PatternSynonyms #-}
module PatternSynonyms where

pattern P :: a -> Maybe (Maybe a)
pattern P a = Just (Just a)

foo :: Maybe (Maybe a) -> Maybe a
foo (P a) = Just a
foo Nothing = Nothing
