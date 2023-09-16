module PermutationSpec where

import Data.Cyclic
import Data.Foldable
import Data.Gemini
import Data.Gemini.Motions
import Data.Group
import Data.Nat
import Data.Permutation
import Gemini.Prelude

import Data.Array as Array
import Data.Map as Map
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, Result(..), (/==), (===))
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)

newtype AnyPermutation n = AnyPermutation (Permutation n)

instance Pos n => Arbitrary (AnyPermutation n) where
  arbitrary = ado
    shuffled <- Gen.shuffle array
    in
      AnyPermutation
        $ Permutation
        $ Map.fromFoldable
        $ Array.zip array shuffled
    where
    array = enumFromTo 0 (knownInt @n - 1)

permutationSpec :: Spec Unit
permutationSpec = do
  describe "Data.Permutation" do
    describe "Group Laws" do
      it "associativity" $ quickCheck $
        \(AnyPermutation x) (AnyPermutation y) (AnyPermutation z) ->
          (x <> y) <> z === (x <> (y <> z) :: Permutation D3)

      it "left identity" $ quickCheck $
        \(AnyPermutation x) ->
          (mempty <> x) === (x :: Permutation D3)

      it "right identity" $ quickCheck $
        \(AnyPermutation x) ->
          (x <> mempty) == (x :: Permutation D3)

      it "left inverse" $ quickCheck $
        \(AnyPermutation x) ->
          (invert x <> x) === (mempty :: Permutation D3)

      it "right inverse" $ quickCheck $
        \(AnyPermutation x) ->
          (x <> invert x) === (mempty :: Permutation D3)
