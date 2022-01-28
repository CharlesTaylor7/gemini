module Data.CyclicSpec where

import           Relude

import           Test.Hspec

import           Data.Cyclic
import           Data.Group  hiding (Cyclic)


spec :: Spec
spec = do
  describe "methods" $ do
    it "smart constructor" $ do
      unCylic @2 (cyclic 3) `shouldBe` 1
    it "compareCyclic" $ do
      ((0 :: Cyclic 6) `compareCyclic` 3) `shouldBe` Opposite

  describe "Num instance" $ do
    it "integer literals" $ do
      unCyclic (3 :: Cyclic 10) `shouldBe` 3
      unCyclic (11 :: Cyclic 10) `shouldBe` 1

    it "addition" $ do
      unCyclic (4 + 9 :: Cyclic 10) `shouldBe` 3

    it "subtraction" $ do
      unCyclic (4 - 9 :: Cyclic 10) `shouldBe` 5

    it "multiplication" $ do
      unCyclic (3 * 7 :: Cyclic 10) `shouldBe` 1

    it "signum" $ do
      -- | always 1
      unCyclic (signum (-4) :: Cyclic 10) `shouldBe` 1

  describe "Group instance" $ do
    it "addition should be the same as the group mappend" $
      2 <> 3 `shouldBe` (2 <> 3 :: Cyclic 17)

    it "subtraction should be the same as the group subtraction" $
      2 - 3 `shouldBe` (2 ~~ 3 :: Cyclic 17)

    it "can fold an empty list" $ do
      unCyclic (fold [] :: Cyclic 3) `shouldBe` 0

    it "can fold a  nonempty list" $ do
      fold ([3, 5, 7] :: [Cyclic 10]) `shouldBe` 5
