{- | Manipulate permutations
-}
module Data.Permutation
  ( Permutation
  , unsafePermutation
  , permute
  , derangements
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Group (class Group)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nat (class Nat, knownInt, natsUnder)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))

newtype Permutation (bound :: Type) = Permutation (Map Int Int)

derive newtype instance Show (Permutation bound)

-- Because the permutation representation is not normalized,
-- we determine if permutations are equal if they map every element of the domain the same way
instance Nat n => Eq (Permutation n) where
  eq p q =
    natsUnder @n
      # Array.all (\k -> permute p k == permute q k)

-- TODO:
-- Either reimplement Permutations as Arrays or make the composition sparse.
-- There's no sense in using a Map with compact representation.
-- Compact meaning, very key of the domain is present in the map.
instance Nat bound => Semigroup (Permutation bound) where
  append p q =
    natsUnder @bound
      # map (\n -> n /\ composed n)
      # Map.fromFoldable
      # Permutation
    where
    composed = permute p >>> permute q

instance Nat n => Monoid (Permutation n) where
  mempty = Permutation Map.empty

instance Nat bound => Group (Permutation bound) where
  invert (Permutation p) =
    p
      # (Map.toUnfoldableUnordered :: _ -> Array _)
      # map Tuple.swap
      # Map.fromFoldable
      # Permutation

-- | Apply permutation to int
permute :: forall n. Permutation n -> Int -> Int
permute (Permutation map) n = map # Map.lookup n # fromMaybe n

-- | Map of changed elements. Elements mapped to themself by the permutation are not present in the map.
derangements :: forall n. Permutation n -> Map Int Int
derangements (Permutation p) = p

unsafePermutation :: forall n. Map Int Int -> Permutation n
unsafePermutation = Permutation
