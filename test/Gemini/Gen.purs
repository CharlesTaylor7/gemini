module Test.Gemini.Gen where

import Data.Gemini
import Data.Permutation
import Partial.Unsafe
import Prelude

import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Foldable (foldMap)
import Data.Location (Location, indexToLocation, location, sibling)
import Data.Map as Map
import Data.Nat (class Nat, class Pos, D50, D54, knownInt)
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

newtype AnyLocation = AnyLocation Location

instance Arbitrary AnyLocation where
  arbitrary = AnyLocation <<< indexToLocation <$> Gen.chooseInt 0 53

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
  arbitrary = apply <$> gen
    where
    apply = AlmostSolvedGemini <<< flip permuteGemini initialGemini

gen :: Gen (Permutation D54)
gen = ado
  t <- toGeminiPermutation <$> transposition
  left <- arbitrary <#> if _ then mirrorLeftRing else mempty
  right <- arbitrary <#> if _ then mirrorRightRing else mempty
  in left <> right <> t

t' :: Gen (Permutation D54)
t' = toGeminiPermutation <$> transposition

left' :: Gen (Permutation D54)
left' = arbitrary <#> if _ then mirrorLeftRing else mempty

right' :: Gen (Permutation D54)
right' = arbitrary <#> if _ then mirrorRightRing else mempty

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

-- | in the initial state, this swaps the Red & Yellow bands
mirrorLeftRing :: Permutation D54
mirrorLeftRing =
  (enumFromTo 0 7 :: Array Int) #
    foldMap (\i -> transpose ((1 - i) `mod` 18) (i + 3))

-- | in the initial state, this swaps the Green & White bands
mirrorRightRing :: Permutation D54
mirrorRightRing =
  (enumFromTo 0 7 :: Array Int) #
    foldMap (\i -> transpose (46 - i) (i + 48))

