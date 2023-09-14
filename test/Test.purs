module Test.Main where

import Gemini.Prelude
import Data.Cyclic
import Data.Nat
import Data.Group
import Data.Gemini
import Data.Gemini.Motions
import Data.Foldable
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, shouldNotSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.QuickCheck
import Test.QuickCheck.Gen as Gen


main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] $ do
  cyclicSpec
  geminiSpec


instance Arbitrary Location where
  arbitrary = Gen.elements inhabitants


geminiSpec :: Spec
geminiSpec = do
  describe "Gemini" $ do
    it "isSolved" $ do
      initialGemini `shouldSatisfy` isSolved
      (initialGemini # applyToGemini (l 3)) `shouldNotSatisfy` isSolved

  describe "Location" $ do
    it "sibling" $ do
      sibling (Location LeftRing 0) `shouldEqual` Nothing
      sibling (Location LeftRing 2) `shouldEqual` Just (Location CenterRing 16)

    it "sibling is its own (partial) inverse" $
      quickCheck $ \x -> case sibling x of
        Nothing -> pure unit
        Just y  -> sibling y `shouldEqual` Just x

cyclicSpec :: Spec Unit
cyclicSpec = do
  describe "Data.Cyclic" $ do
    describe "methods" $ do
      it "creation & destruction" $ do
        let n = unCyclic $ (cyclic 3 :: Cyclic D2)
        n `shouldEqual` 1

      it "compareCyclic" $ do
        ((cyclic @D6 0) `compareCyclic` cyclic 3) `shouldEqual` Opposite
        ((cyclic @D6 0) `compareCyclic` cyclic 6) `shouldEqual` Equal
        ((cyclic @D7 1) `compareCyclic` cyclic 4) `shouldEqual` Precedes
        ((cyclic @D5 1) `compareCyclic` cyclic 4) `shouldEqual` Exceeds

    describe "Ring instance" $ do
      it "addition" $ do
        unCyclic @D10 (cyclic 4 + cyclic 9) `shouldEqual` 3

      it "subtraction" $ do
        unCyclic @D10 (cyclic 4 - cyclic 9) `shouldEqual` 5

      it "multiplication" $ do
        unCyclic @D10 (cyclic 3 * cyclic 7) `shouldEqual` 1

    describe "Group instance" $ do
      it "group operation matches the ring addition group" $ do
        (cyclic 2 <> cyclic 3) `shouldEqual` (cyclic @D16 2 + cyclic 3)

      it "group invert should match the ring subtraction" $ do
        invert (cyclic 2) `shouldEqual` (zero `sub` cyclic 2 :: Cyclic D17)

      it "can fold an empty list" $ do
        unCyclic @D3 (fold []) `shouldEqual` 0

      it "can fold a nonempty list" $ do
        unCyclic @D10 (foldMap cyclic [3, 5, 7]) `shouldEqual` 5

dataCyclicSpec :: Spec Unit
dataCyclicSpec = do
  describe "Data.Cyclic" $ do
    describe "methods" $ do
      it "creation & destruction" $ do
        let n = unCyclic $ (cyclic 3 :: Cyclic D2)
        n `shouldEqual` 1

      it "compareCyclic" $ do
        ((cyclic @D6 0) `compareCyclic` cyclic 3) `shouldEqual` Opposite
        ((cyclic @D6 0) `compareCyclic` cyclic 6) `shouldEqual` Equal
        ((cyclic @D7 1) `compareCyclic` cyclic 4) `shouldEqual` Precedes
        ((cyclic @D5 1) `compareCyclic` cyclic 4) `shouldEqual` Exceeds

    describe "Ring instance" $ do
      it "addition" $ do
        unCyclic @D10 (cyclic 4 + cyclic 9) `shouldEqual` 3

      it "subtraction" $ do
        unCyclic @D10 (cyclic 4 - cyclic 9) `shouldEqual` 5

      it "multiplication" $ do
        unCyclic @D10 (cyclic 3 * cyclic 7) `shouldEqual` 1

    describe "Group instance" $ do
      it "group operation matches the ring addition group" $ do
        (cyclic 2 <> cyclic 3) `shouldEqual` (cyclic @D16 2 + cyclic 3)

      it "group invert matches the ring subtraction" $ do
        invert (cyclic 2) `shouldEqual` (zero `sub` cyclic 2 :: Cyclic D17)

      it "can fold an empty list" $ do
        unCyclic @D3 (fold []) `shouldEqual` 0

      it "can fold a nonempty list" $ do
        unCyclic @D10 (foldMap cyclic [3, 5, 7]) `shouldEqual` 5
