{- | Manipulate permutations, show them in cycle notation
-}
module Permutation
  ( Permutation(..), permute, faithful, domain
  , Cycle, cycle
  , Cycles, cycles, toCycles, fromCycles
  , Semigroup(..)
  , Monoid(..)
  , Group(..)
  ) where

import           Relude                 hiding (break, cycle)

import           Data.Group

import qualified Data.IntMap            as Map
import qualified Data.IntSet            as Set
import qualified Data.List.NonEmpty     as NE
import           Data.Sequence          (Seq ((:<|), (:|>)))
import qualified Data.Sequence          as Seq

import           Optics
import           Optics.State.Operators
import           Prettyprinter          (Pretty (..))
import qualified Prettyprinter          as Pretty


-- | Cycles backed by its cycle notation
newtype Cycles a = Cycles (Seq (Cycle a))
  deriving stock (Functor, Foldable)


newtype Cycle a = Cycle (Seq a)
  deriving stock (Functor, Foldable)


cycle :: Foldable f => f a -> Cycle a
cycle = Cycle . fromList . toList

cycles :: Foldable f => f (Cycle a) -> Cycles a
cycles = Cycles . fromList . toList


-- | Show permutations in cycle notation
instance Pretty a => Pretty (Cycles a) where
  pretty (Cycles cycles) =
    flip foldMap cycles $ \cycle ->
      "(" <>
      (Pretty.hsep $ flip concatMap cycle $ \x -> [pretty x, "->"]) <>
      ")"


permute :: Permutation n -> Int -> Int
permute = applyMap . view #intMap


applyMap :: IntMap Int -> Int -> Int
applyMap map n = map ^? ix n & fromMaybe n


data S = S
  { toVisit   :: !IntSet
  , complete  :: !(Seq (Cycle Int))
  , current   :: !(Seq Int)
  , countdown :: !Int
  }
  deriving stock (Generic)

loop :: Monad m => MaybeT m a -> m ()
loop = void . runMaybeT . forever

break :: Monad m => MaybeT m a
break = MaybeT $ pure $ Nothing


toCycles :: forall bound. KnownNat bound => Permutation bound -> Cycles Int
toCycles p =
  let
    seed = S
      { toVisit = fromList $ natsUnder @bound
      , complete = Seq.Empty
      , current = Seq.Empty
      , countdown = 51
      }
  in
    Cycles $ view #complete $
    flip execState seed $ do
      loop $ do
        let pushCycle current = when (Seq.length current > 1) $ (#complete %= (:|> Cycle current))
        c <- (#countdown <%= subtract 1)
        when (c == 0) $ break

        toVisit <- use #toVisit
        current <- use #current
        when (Set.null toVisit) $ do
          pushCycle current
          break

        when (Seq.length current == 0) $ do
          case Set.minView toVisit of
            Nothing               -> pure ()
            Just (next, toVisit') -> do
              (#toVisit .= toVisit')
              (#current .= next :<| Seq.Empty)

        current <- use #current
        case current of
          Seq.Empty -> break
          (_ :|> last) -> do
            let next = permute p last
            (#toVisit %= Set.delete next)
            case current of
              Seq.Empty -> break
              (first :<| _) -> do
                if first /= last
                then #current %= (:|> next)
                else do
                  pushCycle current
                  (#current .= Seq.Empty)

{--
  In python pseudo code:
  def to_cycles(permutation):
    to_visit = set(permutation)
    cycles = []
    current_cycle = []
    while (true):
      if len(to_visit) == 0:
        if len(current_cycle) > 1:
          cycles.append(current_cycle)
        return cycles

      if len(current_cycle) == 0:
        min_from_set = min(to_visit)
        to_visit.remove(min_from_set)
        current_cycle.append(min_from_set)
        continue

      next = permutation[current_cycle[-1]]
      to_visit.remove(next)
      if next == current_cycle[0]:
        if len(current_cycle) > 1:
          cycles.append(current_cycle)
        current_cycle = []
      else:
        current_cycle.append(next)
--}


fromCycles :: Cycles Int -> Permutation n
fromCycles (Cycles cycles ) =
  flip execState identityPermutation $ do
    for_ cycles $ \cycle ->
      for_ (pairs cycle) $ \(x, y) -> do
        -- update map
        #intMap % at x ?= y

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
natsUnder = [0..n-1]
  where
    n = fromIntegral $ natVal (Proxy :: Proxy bound)


domain :: forall bound. KnownNat bound => Permutation bound -> [Int]
domain _ = natsUnder @bound


newtype Permutation (bound :: Nat) = Permutation
  { intMap :: IntMap Int
  }
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)


instance KnownNat bound => Semigroup (Permutation bound) where
  p <> q = Permutation { intMap }
    where
      intMap =
        flip execState mempty $
          for_ (natsUnder @bound) $ \n ->
            at n ?= (permute p . permute q) n

instance KnownNat n => Monoid (Permutation n) where
  mempty = identityPermutation

identityPermutation = Permutation mempty

instance KnownNat bound => Group (Permutation bound) where
  invert p =
    Permutation $
    flip execState mempty $ do
      for_ (natsUnder @bound) $ \n -> do
        let k = permute p n
        at k ?= n


instance KnownNat bound => Pretty (Permutation bound) where
  pretty = pretty . toCycles
