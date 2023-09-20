module Benchmark.Main where

import Data.Gemini
import Data.Gemini.Solve
import Data.Nat
import Partial.Unsafe
import Prelude
import Test.QuickCheck.Arbitrary

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Data.Array.ST as STArray
import Data.Permutation (Permutation, lift, transpose, transposition)
import Effect (Effect)
import PermutationSpec (AnyPermutation(..))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

main :: Effect Unit
main = runSuite [ solveDetection ]

toGeminiPermutation :: Permutation D50 -> Permutation D54
toGeminiPermutation perm = do
  let extended = lift @D50 @D54 perm
  -- | transpose the extra indices to the end of the index space
  let t1 = transposition @D34 @D50
  let t2 = transposition @D29 @D51
  let t3 = transposition @D47 @D53
  let t = t1 <> t2 <> t3
  let conjugated = t <> extended <> t
  conjugated

scrambled :: Gen Gemini
scrambled = do
  AnyPermutation p <- arbitrary
  let perm = toGeminiPermutation p
  pure $ permuteGemini perm initialGemini

almostSolved :: Gen Gemini
almostSolved =
  transposition <#>
    toGeminiPermutation >>> flip permuteGemini initialGemini
  where
  transposition :: forall n. Nat n => Gen (Permutation n)
  transposition = do
    let max = knownInt @n - 1
    a <- Gen.chooseInt 0 max
    b <- Gen.chooseInt 0 max `Gen.suchThat` notEq a
    pure $ transpose a b

--pure $ permuteGemini (toGeminiPermutation p) initialGemini

solveDetection :: Benchmark
solveDetection = mkBenchmark
  { slug: "benchmark-gemini-solve-detection"
  , title: "Solve Detection"
  , sizes: [ 1, 2 ]
  , sizeInterpretation: ""
  , inputsPerSize: 1000
  , gen:
      case _ of
        1 -> scrambled
        2 -> almostSolved
        _ -> unsafeCrashWith "wat"

  , functions:
      [ benchFn "function name: old" $ isSolved
      , benchFn "function name: new" $ const true
      ]
  }
