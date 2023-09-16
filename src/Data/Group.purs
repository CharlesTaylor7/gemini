module Data.Group
  ( class Group
  , invert
  ) where

import Prelude

class Monoid m <= Group m where
  invert :: m -> m
