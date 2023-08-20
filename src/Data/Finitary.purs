{-# language DefaultSignatures #-}
module Data.Finitary
  ( Finitary(..)
  ) where

import           Relude


class Finitary a where
  inhabitants :: [a]
  default inhabitants :: (Bounded a, Enum a) => [a]
  inhabitants = [minBound..maxBound]
