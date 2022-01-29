module Data.PermutationSpec where

import           Relude                hiding (cycle)

import qualified Data.IntSet           as Set
import           Data.Traversable      (for)

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Data.Permutation


instance KnownNat n => Arbitrary (Permutation n) where
  arbitrary = evalStateT derangement seed & fmap (Permutation . fromList)

    where
      seed :: IntSet
      seed = fromList $ natsUnder @n

      derangement :: StateT IntSet Gen [(Int, Int)]
      derangement = for (natsUnder @n) $ \n -> do
        remaining :: IntSet <- get
        element <- lift $ elements $ Set.toList remaining
        modify $ Set.delete element
        pure (n, element)


spec :: Spec
spec = do
  describe "Group Laws" $ do
    prop "associativity" $ do
      \x y z -> (x <> y) <> z == (x <> (y <> z) :: Permutation 3)

    prop "left identity" $
      \x -> (mempty <> x) == (x :: Permutation 3)

    prop "right identity" $
      \x -> (x <> mempty) == (x :: Permutation 3)

    prop "left inverse" $
      \x -> (invert x <> x) == (mempty :: Permutation 3)

    prop "right inverse" $
      \x -> (x <> invert x) == (mempty :: Permutation 3)

  describe "Cycles" $ do
    it "toCycles" $ do
      (Permutation @3 (fromList [(2, 0), (0, 2)]) & toCycles)
      `shouldBe`
      cycles [cycle [0, 2]]

    it "fromCycles" $ do
      (cycles [cycle [0, 4], cycle [2, 3, 5]] & fromCycles)
      `shouldBe`
      (Permutation @6 (fromList [(0, 4), (1, 1), (2,3), (3,5), (4,0), (5,2)]))

    prop "fromCycles . toCycles == identity" $
      \x -> fromCycles (toCycles x) == (x :: Permutation 10)
