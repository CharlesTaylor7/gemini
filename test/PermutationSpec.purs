module PermutationSpec where

import Gemini.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Permutation (Permutation, unsafePermutation)
import Test.QuickCheck (class Arbitrary, (===))
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

newtype AnyPermutation n = AnyPermutation (Permutation n)

instance Pos n => Arbitrary (AnyPermutation n) where
  arbitrary = ado
    shuffled <- Gen.shuffle array
    in
      AnyPermutation
        $ unsafePermutation
        $ Map.fromFoldable
        $ Array.zip array shuffled
    where
    array = enumFromTo 0 (knownInt @n - 1)

permutationSpec :: Spec Unit
permutationSpec = do
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