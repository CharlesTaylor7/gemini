module Data.CyclicSpec where

import           Relude

import           Test.Hspec

import           Data.Cyclic
import           Data.Group  hiding (Cyclic)


spec :: Spec
spec = do
  describe "methods" $ do
    it "bidirectional pattern synonym" $ do
      let Cyclic n = Cyclic 3 :: Cyclic 2
      n `shouldBe` 1

    it "compareCyclic" $ do
      ((0 :: Cyclic 6) `compareCyclic` 3) `shouldBe` Opposite
      ((0 :: Cyclic 6) `compareCyclic` 6) `shouldBe` Equal
      ((1 :: Cyclic 7) `compareCyclic` 4) `shouldBe` Precedes
      ((1 :: Cyclic 5) `compareCyclic` 4) `shouldBe` Exceeds

  describe "Num instance" $ do
    it "integer literals" $ do
      unCyclic @10 3 `shouldBe` 3
      unCyclic @10 11 `shouldBe` 1

    it "addition" $ do
      unCyclic @10 (4 + 9) `shouldBe` 3

    it "subtraction" $ do
      unCyclic @10 (4 - 9) `shouldBe` 5

    it "multiplication" $ do
      unCyclic @10 (3 * 7) `shouldBe` 1

    it "signum" $ do
      -- | always 1
      unCyclic (signum (-4) :: Cyclic 10) `shouldBe` 1

  describe "Group instance" $ do
    it "addition should be the same as the group mappend" $
      2 <> 3 `shouldBe` (2 <> 3 :: Cyclic 17)

    it "subtraction should be the same as the group subtraction" $
      2 - 3 `shouldBe` (2 ~~ 3 :: Cyclic 17)

    it "can fold an empty list" $ do
      unCyclic @3 (fold []) `shouldBe` 0

    it "can fold a nonempty list" $ do
      unCyclic @10 (fold [3, 5, 7]) `shouldBe` 5
