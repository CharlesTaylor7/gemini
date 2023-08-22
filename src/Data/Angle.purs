module Data.Angle
  ( Angle, AngleUnit
  , sine, cosine, arctan
  , (:*), construct
  , as
  )
  where

import Prelude

import Data.Group (class Group)
import Data.Number as Math

{-
foreign import data Radians :: Type
foreign import data Degrees :: Type
foreign import data Turns :: Type

class AngleUnit (unit :: Type) where
  radiansToUnit :: Number
  unitToRadians :: Number
-}

data AngleUnit 
  = Degrees
  | Radians
  | Turns
  -- ^ How many turns of a circle? e.g. 1 turn is equal to 2 pi radians and 360 degrees.


construct :: Number -> AngleUnit -> Angle
construct x =
  case _ of
    Radians -> Angle x
    Degrees -> Angle $ degreesToRadians x
    Turns -> Angle $ turnsToRadians x

as :: Angle -> AngleUnit -> Number
as (Angle radians) =
  case _ of
    Radians -> radians
    Degrees -> radiansToDegrees radians
    Turns -> radiansToTurns radians


infixr 0 construct as :*
--infixr 0 deconstruct as 

-- | Opaque data type. Angles are internally stored as a 'Number' in radians.
-- Construct or match on the amount of the angle by using one of these bi-directional pattern synonyms:
-- `Radians`, `Degrees`, `Turns`
newtype Angle = Angle Number
instance Semigroup Angle where
  append (Angle a) (Angle b) = Angle (a + b)

instance Monoid Angle where
  mempty = Angle 0.0

instance Group Angle where
  invert (Angle th) = Angle (-th)


-- conversions for degrees
radiansToDegrees :: Number -> Number
radiansToDegrees radians = radians * 180.0 / Math.pi

degreesToRadians :: Number -> Number
degreesToRadians degrees = degrees * Math.pi / 180.0

-- conversions for turns
radiansToTurns :: Number -> Number
radiansToTurns radians = radians / (2.0 * Math.pi)

turnsToRadians :: Number -> Number
turnsToRadians turns = turns * 2.0 * Math.pi


sine :: Angle -> Number
sine (Angle radians) = Math.sin radians


cosine :: Angle -> Number
cosine (Angle radians) = Math.cos radians


arctan :: Number -> Number -> Angle
arctan y x = Angle (Math.atan2 y x)
