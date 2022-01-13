{- | Manipulate permutations, show them in cycle notation
-}
module Permutation
  ( Permutation, permutation, permute
  , Cycles(cycles), toCycles
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


toCycles :: KnownNat n => Permutation n -> Cycles Int
toCycles p =
  let
    seed = S
      { toVisit = fromList [1..(bound p)]
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


{--
fromCycles :: Cycles -> Permutation
fromCycles Cycles { cycles } =
  flip execState mempty $ do
    for_ cycles $ \cycle ->
      for_ (pairs cycle) $ \(x, y) -> do
        -- increase bound item is larger
        #bound %= max x
        -- update map
        #intMap % at x ?= y
--}


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


permutation :: IntMap Int -> Permutation n
permutation intMap = Permutation { intMap }
  -- , bound = maximumOf folded intMap & fromMaybe 0}
  --
bound :: forall n. KnownNat n => Permutation n -> Int
bound _ = fromIntegral $ natVal (Proxy :: Proxy n)

newtype Permutation bound = Permutation
  { intMap :: IntMap Int
  }
  deriving stock (Generic)


instance KnownNat n => Semigroup (Permutation n) where
  p <> q = Permutation { intMap }
    where
      intMap =
        flip execState mempty $
          for_ [1..(bound p)] $ \n ->
            at n ?= (permute p . permute q) n

instance KnownNat n => Monoid (Permutation n) where
  mempty = Permutation { intMap = mempty}
