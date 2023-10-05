module Benchmark.Main where

import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Data.Gemini.Gen (AlmostSolvedGemini(..), ScrambledGemini(..), SolvedGemini(..), arbitrary)
import Data.Gemini.Solve (isSolved, isSolvedFast)
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)

main :: Effect Unit
main = runSuite [ solveDetection ]

solveDetection :: Benchmark
solveDetection = mkBenchmark
  { slug: "benchmark-gemini-solve-detection"
  , title: "Solve Detection"
  , sizes: [ 1, 2, 3 ]
  , sizeInterpretation: "1: scrambled, 2: almost solved, 3: solved"
  , inputsPerSize: 1000
  , gen:
      case _ of
        1 -> arbitrary <#> \(ScrambledGemini g) -> g
        2 -> arbitrary <#> \(AlmostSolvedGemini g) -> g
        3 -> arbitrary <#> \(SolvedGemini g) -> g
        _ -> unsafeCrashWith "wat"
  , functions:
      [ benchFn "old" isSolved
      , benchFn "fast" isSolvedFast
      ]
  }
