module GeminiSpec where

import Data.Gemini
import Data.Gemini.Motions
import Gemini.Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotSatisfy, shouldSatisfy)

geminiSpec :: Spec Unit
geminiSpec = do
  describe "Gemini" $ do
    it "isSolved" $ do
      initialGemini `shouldSatisfy` isSolved
      (initialGemini # applyToGemini (l 3)) `shouldNotSatisfy` isSolved
