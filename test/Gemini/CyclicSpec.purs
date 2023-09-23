module Test.Gemini.CyclicSpec where

import Prelude

import Data.Cyclic (Cyclic, CyclicOrdering(..), compareCyclic, cyclic, unCyclic)
import Data.Foldable (fold, foldMap)
import Data.Group (invert)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = do
  describe "Data.Cyclic" $ do
    describe "methods" $ do
      it "creation & destruction" $ do
        let n = unCyclic $ (cyclic 3 :: Cyclic 2)
        n `shouldEqual` 1

      it "compareCyclic" $ do
        ((cyclic @6 0) `compareCyclic` cyclic 3) `shouldEqual` Opposite
        ((cyclic @6 0) `compareCyclic` cyclic 6) `shouldEqual` Equal
        ((cyclic @7 1) `compareCyclic` cyclic 4) `shouldEqual` Precedes
        ((cyclic @5 1) `compareCyclic` cyclic 4) `shouldEqual` Exceeds

    describe "Ring instance" $ do
      it "addition" $ do
        unCyclic @10 (cyclic 4 + cyclic 9) `shouldEqual` 3

      it "subtraction" $ do
        unCyclic @10 (cyclic 4 - cyclic 9) `shouldEqual` 5

      it "multiplication" $ do
        unCyclic @10 (cyclic 3 * cyclic 7) `shouldEqual` 1

    describe "Group instance" $ do
      it "group operation matches the ring addition group" $ do
        (cyclic 2 <> cyclic 3) `shouldEqual` (cyclic @16 2 + cyclic 3)

      it "group invert mathes the ring subtraction" $ do
        invert (cyclic 2) `shouldEqual` (zero `sub` cyclic @17 2)

      it "can fold an empty list" $ do
        unCyclic @3 (fold []) `shouldEqual` 0

      it "can fold a nonempty list" $ do
        unCyclic @10 (foldMap cyclic [ 3, 5, 7 ]) `shouldEqual` 5
