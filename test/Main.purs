module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Gemini.CyclicSpec as CyclicSpec
import Test.Gemini.GeminiSpec as GeminiSpec
import Test.Gemini.LocationSpec as LocationSpec
import Test.Gemini.PermutationSpec as PermutationSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = launchAff_
  $ runSpec' (defaultConfig { failFast = true }) [ consoleReporter ]
  $ do
      CyclicSpec.spec
      LocationSpec.spec
      PermutationSpec.spec
      GeminiSpec.spec
