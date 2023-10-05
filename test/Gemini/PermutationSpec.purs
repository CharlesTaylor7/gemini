module Test.Gemini.PermutationSpec where

import Gemini.Prelude

import Data.Gemini.Gen (AnyPermutation(..))
import Data.Permutation (Permutation)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec = do
  describe "Data.Permutation" do
    describe "Group Laws" do
      it "left identity" $ quickCheck $
        \(AnyPermutation x) ->
          (mempty <> x) === (x :: Permutation 5)

      it "right identity" $ quickCheck $
        \(AnyPermutation x) ->
          (x <> mempty) === (x :: Permutation 5)

      it "left inverse" $ quickCheck $
        \(AnyPermutation x) ->
          (invert x <> x) === (mempty :: Permutation 5)

      it "right inverse" $ quickCheck $
        \(AnyPermutation x) ->
          (x <> invert x) === (mempty :: Permutation 5)

      it "associativity" $ quickCheck $
        \(AnyPermutation x) (AnyPermutation y) (AnyPermutation z) ->
          (x <> y) <> z === (x <> (y <> z) :: Permutation 5)
