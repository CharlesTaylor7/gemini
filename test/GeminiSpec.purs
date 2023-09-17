module GeminiSpec where

import Gemini.Prelude

import Data.Gemini (applyToGemini, initialGemini)
import Data.Gemini.Motions (l)
import Data.Gemini.Solve (isSolved)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotSatisfy, shouldSatisfy)

geminiSpec :: Spec Unit
geminiSpec = do
  describe "Gemini" $ do
    it "isSolved" $ do
      initialGemini `shouldSatisfy` isSolved
      (initialGemini # applyToGemini (l 3)) `shouldNotSatisfy` isSolved
