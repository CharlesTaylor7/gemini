module Test.Main where

import CyclicSpec
import GeminiSpec
import PermutationSpec
import LocationSpec

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

main :: Effect Unit
main = launchAff_
  $ runSpec' (defaultConfig { failFast = true }) [ consoleReporter ]
  $ do
      cyclicSpec
      locationSpec
      geminiSpec
      permutationSpec
