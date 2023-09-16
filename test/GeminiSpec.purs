module GeminiSpec where

import Data.Cyclic
import Data.Foldable
import Data.Gemini
import Data.Gemini.Motions
import Data.Group
import Data.Nat
import Data.Permutation
import Gemini.Prelude

import Data.Array.NonEmpty as NEArray
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck (class Arbitrary, Result(..), (/==), (===))
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

geminiSpec :: Spec Unit
geminiSpec = do
  describe "Gemini" $ do
    it "isSolved" $ do
      initialGemini `shouldSatisfy` isSolved
      (initialGemini # applyToGemini (l 3)) `shouldNotSatisfy` isSolved
