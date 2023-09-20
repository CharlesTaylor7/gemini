module Benchmark.Main where

import Data.Gemini
import Data.Gemini
import Data.Nat
import Data.Permutation
import Partial.Unsafe
import Prelude
import Test.QuickCheck.Arbitrary

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Data.Array.ST as STArray
import Effect (Effect)
import PermutationSpec (AnyPermutation(..))
import Test.QuickCheck.Gen (Gen)

scrambleGemini :: Gen Gemini
scrambleGemini = do
  AnyPermutation perm <- arbitrary
  let extended = lift @D50 @D54 perm
  -- | transpose the extra indices to the end of the index space
  let t1 = transposition @D34 @D50
  let t2 = transposition @D29 @D51
  let t3 = transposition @D47 @D53
  let t4 = transposition @D52 @D52
  let t = t1 <> t2 <> t3
  let conjugated = t <> extended <> t
  pure $ permuteGemini conjugated initialGemini

main :: Effect Unit
main = runSuite [ solveDetection ]

solveDetection :: Benchmark
solveDetection = mkBenchmark
  { slug: "benchmark-gemini-solve-detection"
  , title: "Solve Detection"
  -- Each `Int` in this `Array` will be the `n` argument in the `gen` field
  , sizes: [ 1000 ]
  , sizeInterpretation: ""
  , inputsPerSize: 10
  , gen: const scrambleGemini
  , functions:
      [ benchFn "function name: foldr" $ identity
      , benchFn "function name: foldmap" $ identity
      ]
  }
