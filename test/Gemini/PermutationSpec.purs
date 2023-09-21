module Test.Gemini.PermutationSpec where

import Gemini.Prelude

import Data.Permutation (Permutation)
import Test.Gemini.Gen (AnyPermutation(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec = do
  describe "Data.Permutation" do
    describe "Group Laws" do
      it "left identity" $ quickCheck $
        \(AnyPermutation x) ->
          (mempty <> x) === (x :: Permutation D5)

      it "right identity" $ quickCheck $
        \(AnyPermutation x) ->
          (x <> mempty) === (x :: Permutation D5)

      it "left inverse" $ quickCheck $
        \(AnyPermutation x) ->
          (invert x <> x) === (mempty :: Permutation D5)

      it "right inverse" $ quickCheck $
        \(AnyPermutation x) ->
          (x <> invert x) === (mempty :: Permutation D5)

      it "associativity" $ quickCheck $
        \(AnyPermutation x) (AnyPermutation y) (AnyPermutation z) ->
          (x <> y) <> z === (x <> (y <> z) :: Permutation D5)
