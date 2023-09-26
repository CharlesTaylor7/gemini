module Test.Gemini.GeminiSpec where

import Prelude

import Data.Gemini (applyToGemini, initialGemini, permuteGemini)
import Data.Gemini.Motions (l)
import Data.Gemini.Solve (isSolved, isSolvedFast)
import Data.Permutation (transpose)
import Random.LCG (mkSeed)
import Test.Gemini.Gen (AlmostSolvedGemini(..), ScrambledGemini(..), SolvedGemini(..))
import Test.QuickCheck (class Arbitrary, Result(..), (/==), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck, quickCheckPure)

spec :: Spec Unit
spec = do
  describe "Solve detection" $ do
    it "initial gemini" $ do
      initialGemini `shouldSatisfy` isSolved
      initialGemini `shouldSatisfy` isSolvedFast

    it "rotate left ring" $ do
      (initialGemini # applyToGemini (l 3)) `shouldNotSatisfy` isSolved
      (initialGemini # applyToGemini (l 3)) `shouldNotSatisfy` isSolvedFast

    it "transpose a red disk with a green one" $ do
      (initialGemini # permuteGemini (transpose 0 45)) `shouldNotSatisfy`
        isSolved

      (initialGemini # permuteGemini (transpose 0 45)) `shouldNotSatisfy`
        isSolvedFast

    describe "property" $ do
      describe "correctness of original" $ do
        it "almost solved" $ quickCheck $
          \(AlmostSolvedGemini g) -> isSolved g === false

        it "solved" $ quickCheck $
          \(SolvedGemini g) -> isSolved g === true

      describe "equivalence of algorithms" $ do
        it "scrambled" $ quickCheck $
          \(ScrambledGemini g) ->
            isSolved g === isSolvedFast g

        it "almost solved" $ quickCheckPure (mkSeed 1847507541) 1 $
          \(AlmostSolvedGemini g) ->
            isSolved g === isSolvedFast g

        it "solved" $ quickCheck $
          \(SolvedGemini g) ->
            isSolved g === isSolvedFast g
