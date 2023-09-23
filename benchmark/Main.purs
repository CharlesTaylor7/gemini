module Benchmark.Main where

import Prelude
import Test.Gemini.Gen

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Data.Gemini (Gemini, initialGemini, permuteGemini)
import Data.Gemini.Solve (isSolved, isSolvedFast)
import Data.Nat (class Nat, D50, D54, knownInt)
import Data.Permutation (Permutation, lift, transpose)
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck.Arbitrary (arbitrary)

main :: Effect Unit
main = runSuite [ solveDetection ]

solveDetection :: Benchmark
solveDetection = mkBenchmark
  { slug: "benchmark-gemini-solve-detection"
  , title: "Solve Detection"
  , sizes: [ 1, 2 ]
  , sizeInterpretation: "1: scrambled, 2: almost solved"
  , inputsPerSize: 1000
  , gen:
      case _ of
        1 -> arbitrary <#> \(ScrambledGemini g) -> g
        2 -> arbitrary <#> \(AlmostSolvedGemini g) -> g
        _ -> unsafeCrashWith "wat"
  , functions:
      [ benchFn "old" isSolved
      , benchFn "fast" isSolvedFast
      ]
  }
