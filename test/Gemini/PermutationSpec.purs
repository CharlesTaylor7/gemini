module Test.Gemini.PermutationSpec where

import Gemini.Prelude

import Data.Permutation (Permutation)
import Test.Gemini.Gen (AnyPermutation(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Util (prop)

spec :: Spec Unit
spec = do
  describe "Data.Permutation" do
    describe "Group Laws" do
      prop "left identity" $
        \(AnyPermutation x) ->
          (mempty <> x) === (x :: Permutation 5)

      prop "right identity" $
        \(AnyPermutation x) ->
          (x <> mempty) === (x :: Permutation 5)

      prop "left inverse" $
        \(AnyPermutation x) ->
          (invert x <> x) === (mempty :: Permutation 5)

      prop "right inverse" $
        \(AnyPermutation x) ->
          (x <> invert x) === (mempty :: Permutation 5)

      prop "associativity" $
        \(AnyPermutation x) (AnyPermutation y) (AnyPermutation z) ->
          (x <> y) <> z === (x <> (y <> z) :: Permutation 5)
