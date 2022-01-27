module Data.Cyclic
  ( Cyclic (..), cyclic
  , compareCyclic, CyclicOrdering(..)
  -- Re export
  , knownInt
  ) where

import           Relude

import           Data.Finitary (Finitary (..))
import           Data.Finite   (finite, getFinite)
import           Data.Group    (Group (..))
import           Utils         (knownInt)


-- | Cyclic group of order n
-- Note: Cyclic has an ord instance because its required to put the data into a map.
-- For relative comparisons, consider  using compareCyclic instead of compare
newtype Cyclic n = Cyclic { unCyclic :: Int }
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData)


-- | smart constructor
cyclic :: forall n. KnownNat n => Int -> Cyclic n
cyclic k = Cyclic $ k `mod` knownInt @n


-- | Group instances
instance KnownNat n => Semigroup (Cyclic n) where
  Cyclic a <> Cyclic b = cyclic $ a + b

instance KnownNat n => Monoid (Cyclic n) where
  mempty = Cyclic 0

instance KnownNat n => Group (Cyclic n) where
  invert (Cyclic k) = cyclic $ -k

-- | Num instance
instance KnownNat n => Num (Cyclic n) where
  (+) = (cyclic .) . ((+) `on` unCyclic)
  (*) = (cyclic .) . ((*) `on` unCyclic)
  abs = identity
  signum _ = 1
  fromInteger = cyclic . fromInteger
  negate = invert

-- | Finitary instance
instance KnownNat n => Finitary (Cyclic n) where
  type Cardinality (Cyclic n) = n
  toFinite = finite . fromIntegral . unCyclic
  fromFinite = cyclic . fromInteger . getFinite

-- How do you order on a elements of a cyclic group?
data CyclicOrdering
  = Precedes
  | Exceeds
  | Equal
  | Opposite
  deriving stock (Eq)


compareCyclic :: forall n. KnownNat n => Cyclic n -> Cyclic n -> CyclicOrdering
compareCyclic a b =
  if | difference == 0                 -> Equal
     | even k && difference == halfway -> Opposite
     | difference <= halfway           -> Precedes
     | otherwise                       -> Exceeds
  where
    k = knownInt @n
    halfway = Cyclic $ k `div` 2
    difference = b - a
