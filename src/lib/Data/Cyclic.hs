module Data.Cyclic
  ( Cyclic, pattern Cyclic, unCyclic
  , compareCyclic, CyclicOrdering(..)
  , distance
  , NonZero
  -- Re export
  , knownInt
  ) where

import           Relude

import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Finitary (Finitary (..))
import           Data.Group    (Group (..))
import           Gemini.Utils  (knownInt, natsUnder)


-- | Cyclic group of order n
-- Note: Cyclic has an ord instance because its required to put the data into a map.
-- For relative comparisons, consider  using compareCyclic instead of compare
newtype Cyclic (n :: Nat) = MkCyclic { unCyclic :: Int }
  deriving stock (Eq, Show, Generic, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (NFData)


-- | Pattern synonym for constructor
pattern Cyclic :: forall n. NonZero n => Int -> Cyclic n
pattern Cyclic k <- MkCyclic k where
  Cyclic k = MkCyclic $ k `mod` knownInt @n
{-# COMPLETE Cyclic #-}
{-# COMPLETE MkCyclic #-}

type NonZero n = (KnownNat n, CmpNat n 0 ~ 'GT)

-- | Group instances
instance NonZero n => Semigroup (Cyclic n) where
  Cyclic a <> Cyclic b = Cyclic $ a + b

instance NonZero n => Monoid (Cyclic n) where
  mempty = Cyclic 0

instance NonZero n => Group (Cyclic n) where
  invert (Cyclic k) = Cyclic $ -k

-- | Num instance
instance NonZero n => Num (Cyclic n) where
  (+) = (Cyclic .) . ((+) `on` unCyclic)
  (*) = (Cyclic .) . ((*) `on` unCyclic)
  abs = identity
  signum _ = 1
  fromInteger = Cyclic . fromInteger
  negate = invert


-- | Finitary instance
instance NonZero n => Finitary (Cyclic n) where
  inhabitants = coerce $ natsUnder @n

-- | Enum instance. have to override everthing. Default definitions are incorrect for this datatype.
instance forall n. NonZero n => Enum (Cyclic n) where
  toEnum = Cyclic
  fromEnum (Cyclic n) = n
  succ n = n + 1
  pred n = n - 1

  enumFromThenTo first next final = ((first :) <$> go next) & fromMaybe []
    where
      step = next - first

      go :: Cyclic n -> Maybe [Cyclic n]
      go current
        | current == final = Just []
        | current == first = Nothing
        | otherwise = (current :) <$> go (current + step)

  enumFrom first = [first]




-- How do you impose order on the elements of a cyclic group?
data CyclicOrdering
  = Precedes
  | Exceeds
  | Equal
  | Opposite
  deriving stock (Eq, Show)


compareCyclic :: forall n. NonZero n => Cyclic n -> Cyclic n -> CyclicOrdering
compareCyclic a b =
  if | difference == 0                 -> Equal
     | even k && difference == halfway -> Opposite
     | difference <= halfway           -> Precedes
     | otherwise                       -> Exceeds
  where
    k = knownInt @n
    halfway = MkCyclic $ k `div` 2
    difference = b - a

distance :: forall n. NonZero n => Cyclic n -> Cyclic n -> Int
distance a b = do
  let Cyclic diff1 = a - b
  let Cyclic diff2 = b - a
  if diff1 < diff2 then diff1 else diff2
