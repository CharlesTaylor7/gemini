module Data.Group
  ( class Group
  , invert
  , pow
  ) where

import Prelude

class
  Monoid m <=
  Group m where
  invert :: m -> m

pow :: forall m. Group m => m -> Int -> m
pow x n = case compare n 0 of
  LT -> invert <<< f x <<< negate $ n
  EQ -> mempty
  GT -> f x n
  where
  f x n
    | n `mod` 2 == 0 = f (x <> x) (n `div` 2)
    | n == 1 = x
    | otherwise = g (x <> x) (n `div` 2) x

  g x n c
    | n `mod` 2 == 0 = g (x <> x) (n `div` 2) c
    | n == 1 = x <> c
    | otherwise = g (x <> x) (n `div` 2) (x <> c)
