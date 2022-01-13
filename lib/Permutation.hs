{- | Manipulate permutations, show them in cycle notation
-}
module Permutation
  ( Permutation, permute, faithful
  , Cycles(cycles), toCycles, fromCycles
  , Semigroup(..)
  , Monoid(..)
  , Group(..)
  ) where

import           Relude                 hiding (break)

import           Data.Group

import qualified Data.IntMap            as Map
import qualified Data.IntSet            as Set
import qualified Data.List.NonEmpty     as NE
import           Data.Sequence          (Seq ((:<|), (:|>)))
import qualified Data.Sequence          as Seq

import           Optics
import           Optics.State.Operators
import           Prettyprinter          (Pretty (..))


-- | Cycles backed by its cycle notation
newtype Cycles a = Cycles { cycles :: Seq (Cycle a)}
  deriving stock (Functor)

type Cycle a = Seq a


-- | Show permutations in cycle notation
instance Pretty a => Pretty (Cycles a) where
  pretty Cycles { cycles } =
    flip foldMap cycles $ \cycle ->
      "(" <> foldMap pretty cycle <> ")"


permute :: Permutation n -> Int -> Int
permute = applyMap . view #intMap


applyMap :: IntMap Int -> Int -> Int
applyMap map n = map ^? ix n & fromMaybe n


data S = S
  { toVisit :: !IntSet
  , cycles  :: !(Seq (Cycle Int))
  , current :: !(Cycle Int)
  }
  deriving stock (Generic)

loop :: Monad m => MaybeT m a -> m ()
loop = void . runMaybeT . forever

break :: Monad m => MaybeT m a
break = MaybeT $ pure $ Nothing


toCycles :: forall n. KnownNat n => Permutation n -> Cycles Int
toCycles p =
  let
    seed = S
      { toVisit = fromList [1..(bound @n)]
      , cycles = Seq.Empty
      , current = Seq.Empty
      }
  in
    Cycles $ view #cycles $
    flip execState seed $ do
      loop $ do
        toVisit <- use #toVisit
        current <- use #current
        when (Set.null toVisit) $ do
          when (Seq.length current > 1) $ do
            #cycles %= (:|> current)
            break

        when (Seq.length current == 0) $ do
          case Set.minView toVisit of
            Nothing               -> pure ()
            Just (next, toVisit') -> do
              #toVisit .= toVisit'
              #current .= next :<| Seq.Empty

        current <- use #current
        case current of
          (_ :|> last) -> do
            let next = permute p last
            #toVisit %= Set.delete next
            case current of
              (first :<| _) -> do
                if first /= last
                then #current %= (:|> next)
                else do
                  when (Seq.length current > 1) $ do
                    #cycles %= (:|> current)
                  #current .= Seq.Empty

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
fromCycles Cycles { cycles } =
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
faithful :: forall n m. (CmpNat m n ~ LT) => Permutation m -> Permutation n
faithful = coerce

bound :: forall n. KnownNat n => Int
bound = fromIntegral $ natVal (Proxy :: Proxy n)

newtype Permutation (n :: Nat) = Permutation
  { intMap :: IntMap Int
  }
  deriving stock (Generic)


instance KnownNat n => Semigroup (Permutation n) where
  p <> q = Permutation { intMap }
    where
      intMap =
        flip execState mempty $
          for_ [1..(bound @n)] $ \n ->
            at n ?= (permute p . permute q) n

instance KnownNat n => Monoid (Permutation n) where
  mempty = identityPermutation

identityPermutation = Permutation mempty

instance KnownNat n => Group (Permutation n) where
  invert p =
    Permutation $
    flip execState mempty $ do
      for_ [1..bound @n] $ \n -> do
        let k = permute p n
        at k ?= n
