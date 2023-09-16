module Data.Point
  ( Point(..)
  , angleToOrigin
  ) where

import Data.Angle
import Data.Group
import Prelude

import Data.Number as Math

newtype Point = Point
  { x :: Number
  , y :: Number
  }

instance Semigroup Point where
  append (Point { x: x1, y: y1 }) (Point { x: x2, y: y2 }) = Point
    { x: x1 + x2, y: y1 + y2 }

instance Monoid Point where
  mempty = Point { x: 0.0, y: 0.0 }

instance Group Point where
  invert (Point { x, y }) = Point { x: -x, y: -y }

-- | Angle the point makes with the origin
angleToOrigin :: Point -> Angle
angleToOrigin (Point { x, y }) = arctan y x
