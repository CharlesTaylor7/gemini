module PermutationSpec where

import Data.Cyclic
import Data.Foldable
import Data.Gemini
import Data.Gemini.Motions
import Data.Group
import Data.Nat
import Data.Permutation
import Gemini.Prelude

import Effect (Effect)
import Test.QuickCheck (class Arbitrary, Result(..), (/==), (===))
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)

newtype AnyPermutation n = AnyPermutation (Permutation n)

instance Pos n => Arbitrary (AnyPermutation n) where
  arbitrary = pure $ AnyPermutation $ (mempty :: Permutation n)

--unsafeCrashWith ""

permutationSpec :: Spec Unit
permutationSpec = do
  describe "Group Laws" $ do
    it "associativity" $ quickCheck $
      \(AnyPermutation x) (AnyPermutation y) (AnyPermutation z) -> (x <> y) <> z
        === (x <> (y <> z) :: Permutation D3)

    it "left identity" $ quickCheck $
      \(AnyPermutation x) -> (mempty <> x) === (x :: Permutation D3)

{-
    it "right identity" $ quickCheck $
      \x -> (x <> mempty) == (x :: Permutation D3)

    it "left inverse" $ quickCheck $
      \x -> (invert x <> x) === (mempty :: Permutation D3)

    it "right inverse" $ quickCheck $
      \x -> (x <> invert x) === (mempty :: Permutation D3)
-}

