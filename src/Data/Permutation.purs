{- | Manipulate permutations, show them in cycle notation
-}
module Data.Permutation
  ( Permutation(..), permute, domain
  , Cycle(..), cycle
  , Cycles(..), cycles, toCycles, fromCycles
  ) where

import Prelude

import Data.Set as Set
import Data.Map as Map
import Data.Nat (Nat, Pos, knownInt, natsUnder, Proxy(..))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList

newtype Permutation (bound :: Type) = Permutation (Map Int Int)

-- Because the permutation representation is not normalized,
-- we determine if permutations are equal if they map every element of the domain the same way
instance Nat n => Eq (Permutation n) where
  eq p q = all 
    (\k -> permute p k == permute q k) 
    (natsUnder (Proxy :: _ n))


instance Nat bound => Semigroup (Permutation bound) where
  append p q = natsUnder (Proxy :: _ bound)
    # map (\n -> Tuple n $ composed n)
    # Map.fromFoldable
    # Permutation
    where
      composed = permute q . permute p

instance Nat n => Monoid (Permutation n) where
  mempty = identityPermutation

identityPermutation :: Permutation n
identityPermutation = Permutation mempty

instance Nat bound => Group (Permutation bound) where
  invert p = natsUnder @bound
    # map (\n -> Tuple (permute p n) n)
    # Map.fromFoldable
    # Permutation


permute :: Permutation n -> Int -> Int
permute (Permutation map) n = map ^? ix n # fromMaybe n


fromCycles :: Cycles Int -> Permutation n
fromCycles (Cycles cycles) = Permutation $ fromList $ concatMap pairs $ cycles

-- | all adjacent pairs in the list plus an extra pair between the last and first item
pairs :: Foldable f => f a -> Tuple a a
pairs x =
  case NonEmptyList.fromFoldable x of
    Nothing      -> []
    Just list ->
      let { head: a, tail: as } = NonEmptyList.uncons list
          go (x:y:rest) = Tuple x y : go (y : rest)
          go (x:Nil)        = Cons (Tuple x a) Nil
          go _          = []
      in go (a:as)



domain :: forall bound. Nat bound => Permutation bound -> Array Int
domain _ = natsUnder (Proxy :: _ bound)
