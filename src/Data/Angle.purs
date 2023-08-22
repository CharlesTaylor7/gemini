module Data.Angle
  ( AngleUnit(..)
  , Angle, Radians, Degrees, Turns
  , sine, cosine, arctan
  )
  where

import           Relude

import           Data.Group


data AngleUnit
  = Radian
  | Degree
  | Turn
  -- ^ How many turns of a circle? e.g. 1 turn is equal to 2 pi radians and 360 degrees.


-- | Opaque data type. Angles are internally stored as a 'Double' in radians.
-- Construct or match on the amount of the angle by using one of these bi-directional pattern synonyms:
-- `Radians`, `Degrees`, `Turns`
newtype Angle = Angle Double
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)
  deriving (Semigroup, Monoid, Group) via (Sum Double)


-- | Construct or match on the radians of an angle
pattern Radians :: Double -> Angle
pattern Radians radians = Angle radians
{-# COMPLETE Radians #-}

-- | Construct or match on the degrees of an angle
pattern Degrees :: Double -> Angle
pattern Degrees degrees <- Angle (radiansToDegrees -> degrees) where
  Degrees degrees = Radians $ degreesToRadians degrees
{-# COMPLETE Degrees #-}

radiansToDegrees :: Double -> Double
radiansToDegrees radians = radians * 180 / pi

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * pi / 180

-- | Construct or match on the turns of an angle
pattern Turns :: Double -> Angle
pattern Turns turns <- Angle (radiansToTurns -> turns) where
  Turns turns = Radians $ turnsToRadians turns
{-# COMPLETE Turns #-}

radiansToTurns :: Double -> Double
radiansToTurns radians = radians / (2 * pi)

turnsToRadians :: Double -> Double
turnsToRadians turns   =  turns * 2 * pi


sine :: Angle -> Double
sine (Radians radians) = sin radians


cosine :: Angle -> Double
cosine (Radians radians) = cos radians


arctan :: Double -> Double -> Angle
arctan y x = Radians $ atan2 y x
