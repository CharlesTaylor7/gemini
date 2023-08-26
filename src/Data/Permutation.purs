{- | Manipulate permutations, show them in cycle notation
-}
module Data.Permutation
  ( Permutation(..), permute, domain
  , Cycle, cycle
  , Cycles, cycles
  -- , toCycles
  , fromCycles
  ) where

import Prelude

import Data.Group (class Group)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (class Foldable)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.Nat (class Nat, knownInt, natsUnder, Proxy(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList

newtype Cycle a = Cycle (Array a)
derive instance Functor Cycle
derive instance Foldable Cycle


newtype Cycles a = Cycles (Array (Cycle a)) 
derive instance Functor Cycles


cycle :: forall f a. Foldable f => f a -> Cycle a
cycle = Cycle <<< Array.fromFoldable

cycles :: forall f a. Foldable f => f (Cycle a) -> Cycles a
cycles = Cycles <<< Array.fromFoldable


newtype Permutation (bound :: Type) = Permutation (Map Int Int)

-- Because the permutation representation is not normalized,
-- we determine if permutations are equal if they map every element of the domain the same way
instance Nat n => Eq (Permutation n) where
  eq p q = Array.all 
    (\k -> permute p k == permute q k) 
    (natsUnder (Proxy :: _ n))


instance Nat bound => Semigroup (Permutation bound) where
  append p q = natsUnder (Proxy :: _ bound)
    # map (\n -> n /\ composed n)
    # Map.fromFoldable
    # Permutation
    where
      composed = permute q <<< permute p

instance Nat n => Monoid (Permutation n) where
  mempty = identityPermutation

identityPermutation :: forall n. Permutation n
identityPermutation = Permutation Map.empty

instance Nat bound => Group (Permutation bound) where
  invert p = natsUnder (Proxy :: _ bound)
    # map (\n -> permute p n /\ n)
    # Map.fromFoldable
    # Permutation


permute :: forall n. Permutation n -> Int -> Int
permute (Permutation map) n = map # Map.lookup n # fromMaybe n


fromCycles :: forall n. Cycles Int -> Permutation n
fromCycles (Cycles cycles) = 
  Permutation $ 
    Map.fromFoldable $ 
      List.concatMap pairs $ 
        List.fromFoldable cycles


-- | all adjacent pairs in the list plus an extra pair between the last and first item
pairs :: forall f a. Foldable f => f a -> List (a /\ a)
pairs x =
  case NonEmptyList.fromFoldable x of
    Nothing      -> Nil
    Just list ->
      let { head: a, tail: as } = NonEmptyList.uncons list
          go (Cons x (Cons y rest)) = (x /\ y) : go (y : rest)
          go (Cons x Nil)        = (x /\ a) : Nil
          go _          = Nil
      in go (a:as)



domain :: forall bound. Nat bound => Permutation bound -> Array Int
domain _ = natsUnder (Proxy :: _ bound)