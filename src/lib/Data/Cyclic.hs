module Data.Cyclic
  ( Cyclic, pattern Cyclic, unCyclic
  , compareCyclic, CyclicOrdering(..)
  -- Re export
  , knownInt
  ) where

import           Relude

import           Data.Finitary (Finitary (..))
import           Data.Group    (Group (..))
import           Utils         (knownInt, natsUnder)


-- | Cyclic group of order n
-- Note: Cyclic has an ord instance because its required to put the data into a map.
-- For relative comparisons, consider  using compareCyclic instead of compare
newtype Cyclic (n :: Nat) = MkCyclic { unCyclic :: Int }
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData)

-- | Pattern synonym for constructor
pattern Cyclic :: forall n. KnownNat n => Int -> Cyclic n
pattern Cyclic k <- MkCyclic k where
  Cyclic k = MkCyclic $ k `mod` knownInt @n
{-# COMPLETE Cyclic #-}
{-# COMPLETE MkCyclic #-}


-- | Group instances
instance KnownNat n => Semigroup (Cyclic n) where
  Cyclic a <> Cyclic b = Cyclic $ a + b

instance KnownNat n => Monoid (Cyclic n) where
  mempty = Cyclic 0

instance KnownNat n => Group (Cyclic n) where
  invert (Cyclic k) = Cyclic $ -k

-- | Num instance
instance KnownNat n => Num (Cyclic n) where
  (+) = (Cyclic .) . ((+) `on` unCyclic)
  (*) = (Cyclic .) . ((*) `on` unCyclic)
  abs = identity
  signum _ = 1
  fromInteger = Cyclic . fromInteger
  negate = invert


-- | Finitary instance
instance KnownNat n => Finitary (Cyclic n) where
  inhabitants = coerce $ natsUnder @n


-- How do you order on a elements of a cyclic group?
data CyclicOrdering
  = Precedes
  | Exceeds
  | Equal
  | Opposite
  deriving stock (Eq, Show)


compareCyclic :: forall n. KnownNat n => Cyclic n -> Cyclic n -> CyclicOrdering
compareCyclic a b =
  if | difference == 0                 -> Equal
     | even k && difference == halfway -> Opposite
     | difference <= halfway           -> Precedes
     | otherwise                       -> Exceeds
  where
    k = knownInt @n
    halfway = MkCyclic $ k `div` 2
    difference = b - a
