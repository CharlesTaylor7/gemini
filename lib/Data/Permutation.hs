{- | Manipulate permutations, show them in cycle notation
-}
module Data.Permutation
  ( Permutation(..), permute, faithful, domain, natsUnder
  , Cycle(..), cycle
  , Cycles(..), cycles, toCycles, fromCycles
  -- re exports
  , knownInt
  , Semigroup(..)
  , Monoid(..)
  , Group(..)
  ) where

import           Relude        hiding (break, cycle)

import           Data.Group

import qualified Data.IntSet   as Set
import           Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq

import           Optics
import           Prettyprinter (Pretty (..))
import qualified Prettyprinter as Pretty

import           Utils         (knownInt)


newtype Cycles a = Cycles { uncycles :: (Seq (Cycle a)) }
  deriving stock (Eq, Generic, Show, Functor)
  deriving anyclass (NFData)


newtype Cycle a = Cycle (Seq a)
  deriving stock (Eq, Generic, Show)
  deriving newtype (Functor, Foldable, FoldableWithIndex Int)
  deriving anyclass (NFData)


cycle :: Foldable f => f a -> Cycle a
cycle = Cycle . fromList . toList

cycles :: Foldable f => f (Cycle a) -> Cycles a
cycles = Cycles . fromList . toList


-- | Show permutations in cycle notation
-- instance Pretty a => Pretty (Cycles a) where
    -- pretty = prettyList . toList . uncycles

instance Pretty a => Pretty (Cycle a) where
    pretty cycle
      =  "("
      <> (Pretty.sep $ map pretty $ toList cycle)
      <> ")"


permute :: Permutation n -> Int -> Int
permute (Permutation map) n = map ^? ix n & fromMaybe n


-- | Set patterns
pattern SetMin :: Set.Key -> IntSet -> IntSet
pattern SetMin min view <- (Set.minView -> Just (min, view))

pattern SetEmpty :: IntSet
pattern SetEmpty <- (Set.minView -> Nothing)

-- | Accumulator for toCycles implementation
data S = S
  { toVisit  :: !IntSet
  , complete :: !(Seq (Cycle Int))
  , current  :: !(Seq Int)
  }
  deriving stock (Generic)


toCycles :: forall bound. KnownNat bound => Permutation bound -> Cycles Int
toCycles p = go seed
  where
    seed = S
      { toVisit = fromList $ natsUnder @bound
      , complete = Seq.Empty
      , current = Seq.Empty
      }

    pushCycle :: S -> S
    pushCycle s@S { current } = s
      & #current .~ Seq.Empty
      & if Seq.length current < 2
        then identity
        else #complete %~ (:|> Cycle current)

    go :: S -> Cycles Int
    go s@S { toVisit = SetEmpty }                                = Cycles $ view #complete $ pushCycle s
    go s@S { toVisit = SetMin min toVisit, current = Seq.Empty } = go s { toVisit, current = fromList [min] }
    go s@S { current }                                           = do
      -- these patterns are exhaustive when you take into account the previous two pattern guards
      let head = case current of h :<| _ -> h
      let tail = case current of _ :|> t -> t
      let next = permute p tail
      if head == next
      then go $ pushCycle s
      else go $ s
        & #current %~ (:|> next)
        & #toVisit %~ Set.delete next


fromCycles :: Cycles Int -> Permutation n
fromCycles (Cycles cycles ) = Permutation $ fromList $ concatMap pairs $ cycles

-- | all adjacent pairs in the list plus an extra pair between the last and first item
pairs :: Foldable f => f a -> [(a, a)]
pairs x =
  case nonEmpty $ toList x of
    Nothing      -> []
    Just (a:|as) ->
      let go (x:y:rest) = (x, y) : go (y : rest)
          go [x]        = [(x, a)]
          go _          = []
      in go (a:as)


-- | Faithfully promote a permutation into a larger group
faithful :: forall n m. (CmpNat m n ~ 'LT) => Permutation m -> Permutation n
faithful = coerce


natsUnder :: forall bound. KnownNat bound => [Int]
natsUnder = [0..knownInt @bound - 1]


domain :: forall bound. KnownNat bound => Permutation bound -> [Int]
domain _ = natsUnder @bound

-- Permutation definition and instances
newtype Permutation (bound :: Nat) = Permutation
  { intMap :: IntMap Int
  }
  deriving stock (Generic, Show)
  deriving newtype (NFData)


-- Because the permutation representation is not normalized,
-- we determine if permutations are equal if they map every element of the domain the same way
instance KnownNat n => Eq (Permutation n) where
  p == q = all (\k -> permute p k == permute q k) (natsUnder @n)


instance KnownNat bound => Semigroup (Permutation bound) where
  p <> q = natsUnder @bound
    & map (\n -> (n, composed n))
    & fromList
    & Permutation
    where
      composed = permute q . permute p

instance KnownNat n => Monoid (Permutation n) where
  mempty = identityPermutation

identityPermutation :: Permutation n
identityPermutation = Permutation mempty

instance KnownNat bound => Group (Permutation bound) where
  invert p = natsUnder @bound
    & map (\n -> (permute p n, n))
    & fromList
    & Permutation
