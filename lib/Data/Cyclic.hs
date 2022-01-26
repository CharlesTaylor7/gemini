module Data.Cyclic
  ( Cyclic (..), cyclic, knownInt
  ) where

import           Relude

import           Data.Finitary (Finitary (..))
import           Data.Finite   (finite, getFinite)
import           Data.Group    (Group (..))
import           Utils         (knownInt)


-- | Cyclic group of order n
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
