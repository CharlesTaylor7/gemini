{- | Manipulate permutations
-}
module Data.Permutation
  ( Permutation
  , unsafePermutation
  , permute
  , derangements
  , lift
  , transpose
  , cycle
  , module Data.Group
  ) where

import Prelude

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
import Data.Nat (class Compare, class Nat, LT, knownInt, natsUnder)
import Data.Reflectable (class Reflectable)
import Data.Set as Set
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))

newtype Permutation (bound :: Int) = Permutation (Map Int Int)

derive newtype instance Show (Permutation bound)

-- Because the permutation representation is not normalized,
-- we determine if permutations are equal if they map every element of the domain the same way

instance Eq (Permutation n) where
  eq p q =
    p <> invert q
      # isIdentity
    where
    isIdentity p =
      derangements p
        # Map.toUnfoldableUnordered
        # Array.all (\(i /\ j) -> i == j)

-- O(m * log n + n * log m) < O(k * log k) where k = max(m,n)
-- where m is the size of the first permutation
-- and m is the size of the second permutation
instance Semigroup (Permutation bound) where
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

instance Monoid (Permutation n) where
  mempty = Permutation Map.empty

instance Group (Permutation n) where
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

lift :: forall @a @b. Compare a b LT => Permutation a -> Permutation b
lift = derangements >>> unsafePermutation

transpose :: forall @c. Int -> Int -> Permutation c
transpose a b = unsafePermutation $
  Map.fromFoldable
    [ a /\ b
    , b /\ a
    ]

cycle :: forall @c. Array Int -> Permutation c
cycle input = unsafePermutation
  $ Map.fromFoldable
  $ pairs input

-- | all adjacent pairs in the list plus an extra pair between the last and first item
pairs :: forall a f. Foldable f => f a -> List (a /\ a)
pairs x =
  case NonEmptyList.fromFoldable x of
    Nothing -> List.Nil
    Just list ->
      let
        { head: a, tail: as } = NonEmptyList.uncons list
        go (x : y : rest) = (x /\ y) : go (y : rest)
        go (x : List.Nil) = (x /\ a) : List.Nil
        go _ = List.Nil
      in
        go (a : as)
