module Test.Gemini.GeminiSpec where

import Gemini.Prelude

import Data.Gemini (applyToGemini, initialGemini)
import Data.Gemini.Motions (l)
import Data.Gemini.Solve (isSolved, isSolvedFast)
import Data.Permutation (transpose)
import Test.Gemini.Gen (ScrambledGemini(..))
import Test.QuickCheck (class Arbitrary, Result(..), (/==), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec = do
  describe "Gemini" $ do
    describe "isSolved" $ do
      it "initial gemini" $ do
        initialGemini `shouldSatisfy` isSolved
        initialGemini `shouldSatisfy` isSolvedFast

      it "rotate left ring" $ do
        -- | rotate the left ring
        (initialGemini # applyToGemini (l 3)) `shouldNotSatisfy` isSolved
        (initialGemini # applyToGemini (l 3)) `shouldNotSatisfy` isSolvedFast

      it "transpose a red disk with a green one" $ do
        (initialGemini # permuteGemini (transpose 0 45)) `shouldNotSatisfy`
          isSolved

        (initialGemini # permuteGemini (transpose 0 45)) `shouldNotSatisfy`
          isSolvedFast

    it "property test" $ quickCheck $
      \(ScrambledGemini g) ->
        isSolved g === isSolvedFast g
