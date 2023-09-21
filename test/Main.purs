module Test.Main where

import CyclicSpec
import GeminiSpec
import LocationSpec
import PermutationSpec
import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
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
