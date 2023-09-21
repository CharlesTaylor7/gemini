module GeminiSpec where

import Gemini.Prelude
import Test.Gemini.Gen

import Data.Gemini (applyToGemini, initialGemini)
import Data.Gemini.Motions (l)
import Data.Gemini.Solve (isSolved, isSolvedFast)
import Test.QuickCheck (class Arbitrary, Result(..), (/==), (===))
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)

geminiSpec :: Spec Unit
geminiSpec = do
  describe "Gemini" $ do
    it "isSolved" $ do
      initialGemini `shouldSatisfy` isSolved
      (initialGemini # applyToGemini (l 3)) `shouldNotSatisfy` isSolved
    it "property test" $ quickCheck $
      \(ScrambledGemini g) ->
        isSolved g === isSolvedFast g
