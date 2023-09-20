{- | Manipulate permutations
-}
module Data.Permutation
  ( Permutation
  , unsafePermutation
  , permute
  , derangements
  , transposition
  , lift
  , module Data.Group
  ) where

import Partial.Unsafe
import Prelude
import Prim.TypeError

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.Group (class Group, invert)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nat (class Nat, class Lt, knownInt, natsUnder)
import Data.Set as Set
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

-- O(m * log n + floor(n - m) * log m) < O(k * log k) where k = max(m,n)
-- where m is the size of the first permutation
-- and m is the size of the second permutation
instance Nat bound => Semigroup (Permutation bound) where
  append (Permutation p) perm2@(Permutation q) =
    -- fold over p and lookup composed image)
    -- also delete elements from q, and insert the remanant of q, into the end result
    let
      worker k v acc =
        { p: acc.p # Map.update
            ( \_ ->
                let
                  qv = permute perm2 v
                in
                  if k == qv then Nothing else Just qv
            )
            k
        , q: acc.q # Map.delete v
        }
      { p, q } = foldrWithIndex worker { p, q } p
    in
      unsafePermutation $
        Map.union p q

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

lift :: forall @a @b. Lt a b => Permutation a -> Permutation b
lift = derangements >>> unsafePermutation

transposition :: forall @a @b @c. Lt a c => Lt b c => Nat a => Nat b => Permutation c
transposition = unsafeTranspose (knownInt @a) (knownInt @b)
  where
  unsafeTranspose a b = unsafePermutation $
    Map.fromFoldable
      [ a /\ b
      , b /\ a
      ]


class NotEqual a b
instance Fail (Text "") => NotEqual a a
