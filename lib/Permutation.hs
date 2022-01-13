{- | Manipulate permutations, show them in cycle notation
-}
module Permutation
  ( Permutation, permutation, apply
  , Cycles(cycles), toCycles, fromCycles
  ) where

import           Relude                    hiding (break)

import qualified Data.IntSet               as Set
import qualified Data.List.NonEmpty        as NE
import           Data.Sequence             (Seq ((:<|), (:|>)))
import qualified Data.Sequence             as Seq

import           Optics
import           Optics.State.Operators
import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty


-- | Cycles backed by its cycle notation
newtype Cycles = Cycles { cycles :: Seq Cycle }
type Cycle = Seq Int


-- | Show permutations in cycle notation
instance Pretty Cycles where
  pretty Cycles { cycles } =
    flip foldMap cycles $ \cycle ->
      "(" <> foldMap Pretty.unsafeViaShow cycle <> ")"


apply :: Permutation -> Int -> Int
apply = applyMap . view #intMap


applyMap :: IntMap Int -> Int -> Int
applyMap map n = map ^? ix n & fromMaybe n


data S = S
  { toVisit :: !IntSet
  , cycles  :: !(Seq Cycle)
  , current :: !Cycle
  }
  deriving stock (Generic)

loop :: Monad m => MaybeT m a -> m ()
loop = void . runMaybeT . forever

break :: Monad m => MaybeT m a
break = MaybeT $ pure $ Nothing


toCycles :: Permutation -> Cycles
toCycles Permutation { bound, intMap } =
  let
    permute = applyMap intMap
    seed = S
      { toVisit = fromList [1..bound]
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
            let next = permute last
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



fromCycles :: Cycles -> Permutation
fromCycles Cycles { cycles } =
  flip execState mempty $ do
    for_ cycles $ \cycle ->
      for_ (pairs cycle) $ \(x, y) -> do
        -- increase bound item is larger
        #bound %= max x
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


permutation :: IntMap Int -> Permutation
permutation intMap = Permutation { intMap, bound = maximumOf folded intMap & fromMaybe 0}

data Permutation = Permutation
  { intMap :: !(IntMap Int)
  , bound  :: !Int
  }
  deriving stock (Generic)


instance Semigroup Permutation where
  Permutation { bound = m, intMap = p } <> Permutation { bound = n, intMap = q } =
    Permutation { bound , intMap }
    where
      bound = max m n
      intMap =
        flip execState mempty $
          for_ [1..bound] $ \n ->
            at n ?= (applyMap p . applyMap q) n

instance Monoid Permutation where
  mempty = Permutation { intMap = mempty, bound = 0 }
