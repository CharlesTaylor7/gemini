{- | Manipulate permutations, show them in cycle notation
-}
module Permutation
  ( Permutation, permutation, apply
  , cycles, toCycles, fromCycles
  ) where

import           Relude

import qualified Data.List.NonEmpty        as NE
import           Data.Sequence             (Seq ((:<|), (:|>)))
import qualified Data.Sequence             as Seq

import           Optics
import           Optics.State.Operators
import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty


-- | Cycles backed by its cycle notation
newtype Cycles = Cycles { cycles :: [Cycle] }
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

toCycles :: Permutation -> Cycles
toCycles Permutation { intMap } = go (Cycles []) Empty 1
  where
    go :: Cycles -> Cycle -> Int -> Cycles
    go acc cycle n = _

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
