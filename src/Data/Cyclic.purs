module Data.Cyclic
  ( Cyclic, cyclic, unCyclic
  --, compareCyclic, CyclicOrdering(..)
  , distance
  ) where

import           Prelude

import           Data.Finitary (class Finitary)
import           Data.Group    (class Group)
import           Data.Nat      (class Nat, class Pos, Proxy(..), knownInt, natsUnder)
import Safe.Coerce (coerce)


-- | Cyclic group of order n
-- Note: Cyclic has an ord instance because its required to put the data into a map.
-- For relative comparisons, consider  using compareCyclic instead of compare
newtype Cyclic (n :: Type) = MkCyclic Int

unCyclic :: forall n. Cyclic n -> Int
unCyclic (MkCyclic x) = x

cyclic :: forall n. Pos n => Int -> Cyclic n
cyclic k = MkCyclic $ k `mod` (knownInt (Proxy :: _ n))

-- | Semiring instance
instance Pos n => Semiring (Cyclic n) where
  zero = MkCyclic 0
  one = cyclic 1
  add (MkCyclic x) (MkCyclic y) = cyclic $ x + y
  mul (MkCyclic x) (MkCyclic y) = cyclic $ x * y


instance Pos n => Ring (Cyclic n) where
  sub (MkCyclic x) (MkCyclic y) = cyclic $ x - y

{-
-- | Finitary instance
instance Pos n => Finitary (Cyclic n) where
  inhabitants = coerce $ natsUnder @n
  -}

-- How do you impose order on the elements of a cyclic group?
data CyclicOrdering
  = Precedes
  | Exceeds
  | Equal
  | Opposite
derive instance Eq CyclicOrdering

compareCyclic :: forall n. Pos n => Cyclic n -> Cyclic n -> CyclicOrdering
compareCyclic a b =
  let
    k = knownInt (Proxy :: _ n)
    halfway = MkCyclic $ k `div` 2
    difference = b - a
  in 
    if difference == 0  then Equal
    else if even k && difference == halfway then Opposite
    else if difference <= halfway           then Precedes
    else Exceeds

even :: Int -> Boolean
even = (_ `mod` 2) >>> eq 0

distance :: forall n. Pos n => Cyclic n -> Cyclic n -> Int
distance a b = do
  let MkCyclic diff1 = a - b
  let MkCyclic diff2 = b - a
  if diff1 < diff2 then diff1 else diff2
