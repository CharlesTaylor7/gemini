module Data.Point
  ( Point(..)
  , norm, angleToOrigin
  )
  where

import           Relude

import           Data.Angle
import           Data.Group
import           Prettyprinter (Pretty (..))


data Point = Point
  { x :: !Double
  , y :: !Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)


instance Pretty Point where
  pretty (Point x y)
    = "("
    <> pretty x
    <> ", "
    <> pretty y
    <> ")"

instance Semigroup Point where
  Point x1 y1 <> Point x2 y2 = Point (x1 + x2) (y1 + y2)

instance Monoid Point where
  mempty = Point 0 0

instance Group Point where
  invert (Point x y) = Point (-x) (-y)


-- ## Operations
-- | Distance to the origin
norm :: Point -> Double
norm Point { x, y } = sqrt $ x*x + y*y

-- | Angle the point makes with the origin
angleToOrigin :: Point -> Angle
angleToOrigin (Point x y) = arctan y x
