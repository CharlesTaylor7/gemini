module Data.Finitary
  ( class Finitary, inhabitants
  , boundedEnumInhabitants
  ) where

import Prelude
import Data.Enum (class Enum, enumFromTo)

class Finitary a where
  inhabitants :: Array a


boundedEnumInhabitants :: forall a. Bounded a => Enum a => Array a
boundedEnumInhabitants = enumFromTo bottom top
