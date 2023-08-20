module Data.Finitary
  ( class Finitary, inhabitants
  ) where

import Prelude
import Data.Enum (class Enum, enumFromTo)

class Finitary a where
  inhabitants :: Array a

instance (Bounded a, Enum a) => Finitary a where
  inhabitants = enumFromTo bottom top
