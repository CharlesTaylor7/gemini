{-# language MultiParamTypeClasses #-}
module Data.Angle
  ( AngleUnit(..)
  -- | Angle uses smart constructors and destructors to ensure units are handled appropriately
  , Angle, pattern Radians, pattern Degrees, pattern Turns

  -- trigonometic functions
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

-- | Construct or match on the radians of an angle
newtype Angle = Radians Double
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)
  deriving (Semigroup, Monoid, Group) via (Sum Double)


-- | Construct or match on the degrees of an angle
pattern Degrees :: Double -> Angle
pattern Degrees degrees <- Radians (radiansToDegrees -> degrees) where
  Degrees degrees = Radians $ degreesToRadians degrees
{-# COMPLETE Degrees #-}

radiansToDegrees radians = radians * 180 / pi
degreesToRadians degrees = degrees * pi / 180

-- | Construct or match on the turns of an angle
pattern Turns :: Double -> Angle
pattern Turns turns <- Radians (radiansToTurns -> turns) where
  Turns turns = Radians $ turnsToRadians turns
{-# COMPLETE Turns #-}

radiansToTurns radians = radians / (2 * pi)
turnsToRadians turns   =  turns * 2 * pi


sine :: Angle -> Double
sine (Radians radians) = sin radians


cosine :: Angle -> Double
cosine (Radians radians) = cos radians


arctan :: Double -> Double -> Angle
arctan y x = Radians $ atan2 y x
