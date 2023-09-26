module Test.Gemini.GeminiSpec where

import Prelude

import Data.Gemini (applyToGemini, initialGemini, permuteGemini)
import Data.Gemini.Motions (l)
import Data.Gemini.Solve (isSolved, isSolvedFast)
import Data.Permutation (transpose)
import Test.Gemini.Gen (AlmostSolvedGemini(..), ScrambledGemini(..), SolvedGemini(..))
import Test.QuickCheck (class Arbitrary, Result(..), (/==), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Util (prop)

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
        prop "almost solved" $
          \(AlmostSolvedGemini g) -> isSolved g === false

        prop "solved" $
          \(SolvedGemini g) -> isSolved g === true

      describe "equivalence of algorithms" $ do
        prop "scrambled" $
          \(ScrambledGemini g) ->
            isSolved g === isSolvedFast g

        prop "almost solved" $
          \(AlmostSolvedGemini g) ->
            isSolved g === isSolvedFast g

        prop "solved" $
          \(SolvedGemini g) ->
            isSolved g === isSolvedFast g
