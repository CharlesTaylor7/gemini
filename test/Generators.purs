module Test.Gemini.Gen where

import Data.Gemini
import Data.Permutation
import Prelude

import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Map as Map
import Data.Nat (class Nat, class Pos, D50, D54, knownInt)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

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

newtype ScrambledGemini = ScrambledGemini Gemini

instance Arbitrary ScrambledGemini where
  arbitrary = do
    AnyPermutation p <- arbitrary
    let perm = toGeminiPermutation p
    pure $ ScrambledGemini $ permuteGemini perm initialGemini

newtype AlmostSolvedGemini = AlmostSolvedGemini Gemini

instance Arbitrary AlmostSolvedGemini where
  arbitrary = do
    transposition
      <#> toGeminiPermutation
        >>> flip permuteGemini initialGemini
        >>> AlmostSolvedGemini
    where
    transposition :: forall n. Nat n => Gen (Permutation n)
    transposition = do
      let max = knownInt @n - 1
      a <- Gen.chooseInt 0 max
      b <- Gen.chooseInt 0 max `Gen.suchThat` notEq a
      pure $ transpose a b

toGeminiPermutation :: Permutation D50 -> Permutation D54
toGeminiPermutation perm = do
  let extended = lift @D50 @D54 perm
  -- | transpose the extra indices to the end of the index space
  let t1 = transpose 34 50
  let t2 = transpose 29 51
  let t3 = transpose 47 53
  let t = t1 <> t2 <> t3
  let conjugated = t <> extended <> t
  conjugated

