module Test.Gemini.Gen
  ( module ReExport
  , AnyLocation(..)
  , AnyPermutation(..)
  , AnyTransposition(..)
  , ScrambledGemini(..)
  , SolvedGemini(..)
  , AlmostSolvedGemini(..)
  ) where

import Data.Gemini
import Data.Permutation
import Partial.Unsafe
import Prelude

import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Finitary (inhabitants)
import Data.Foldable (fold, foldMap)
import Data.Location (Location, indexToLocation)
import Data.Location (Ring(..))
import Data.Map as Map
import Data.Nat (class Pos, D50, D54, knownInt)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck (arbitrary) as ReExport
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

newtype AnyTransposition n = AnyTransposition (Permutation n)

instance Pos n => Arbitrary (AnyTransposition n) where
  arbitrary = AnyTransposition <$> do
    let max = knownInt @n - 1
    a <- Gen.chooseInt 0 max
    b <- Gen.chooseInt 0 max `Gen.suchThat` notEq a
    pure $ transpose a b

newtype SolvedGemini = SolvedGemini Gemini

instance Arbitrary SolvedGemini where
  arbitrary = SolvedGemini <<< permuteInitial <$> do
    horizontal <- arbitrary
    let h = if horizontal then mirrorHorizontal else mempty
    let mirrorLeftRing = if horizontal then mirrorLeftRing2 else mirrorLeftRing1
    let
      mirrorRightRing =
        if horizontal then mirrorRightRing2 else mirrorRightRing1
    left <- arbitrary <#> if _ then mirrorLeftRing else mempty
    right <- arbitrary <#> if _ then mirrorRightRing else mempty
    pure $ h <> left <> right

newtype ScrambledGemini = ScrambledGemini Gemini

instance Arbitrary ScrambledGemini where
  arbitrary = ado
    AnyPermutation p <- arbitrary
    let perm = toGeminiPermutation p
    in ScrambledGemini $ permuteInitial perm

newtype AlmostSolvedGemini = AlmostSolvedGemini Gemini

instance Arbitrary AlmostSolvedGemini where
  arbitrary = AlmostSolvedGemini <$> ado
    (AnyTransposition t) <- arbitrary
    let p = toGeminiPermutation t
    (SolvedGemini g) <- arbitrary
    in permuteGemini p g

permuteInitial :: Permutation D54 -> Gemini
permuteInitial p = permuteGemini p initialGemini

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

-- | mirror left ring along a diagonal
-- | in the initial state, this swaps the Red & Yellow bands
mirrorLeftRing1 :: Permutation D54
mirrorLeftRing1 =
  (enumFromTo 0 7 :: Array Int)
    # foldMap (\i -> transpose ((1 - i) `mod` 18) (i + 3))

mirrorLeftRing2 :: Permutation D54
mirrorLeftRing2 =
  (enumFromTo 0 7 :: Array Int)
    # foldMap (\i -> transpose ((6 - i) `mod` 18) (i + 8))

-- | mirror right ring along a diagonal
-- | in the initial state, this swaps the Green & White bands
mirrorRightRing1 :: Permutation D54
mirrorRightRing1 =
  fold
    [ transpose 46 48
    , transpose 45 49
    , transpose 44 50
    , transpose 43 51
    , transpose 42 20
    , transpose 41 53
    , transpose 40 36
    , transpose 39 37
    ]

-- | mirror right ring along a diagonal
-- | in the initial state, this swaps the Green & White bands
mirrorRightRing2 :: Permutation D54
mirrorRightRing2 =
  fold
    [ transpose 51 53
    , transpose 50 36
    , transpose 49 37
    , transpose 48 38
    , transpose 25 39
    , transpose 46 40
    , transpose 45 41
    , transpose 44 42
    ]

-- | swap every disk along the horizontal axis
mirrorHorizontal :: Permutation D54
mirrorHorizontal = foldMap mirror inhabitants
  where
  mirror :: Ring -> Permutation D54
  mirror ring =
    (enumFromTo 0 8 :: Array _)
      # foldMap
          ( \i -> transpose
              (ringOffset ring + (14 + i) `mod` 18)
              (ringOffset ring + (13 - i) `mod` 18)
          )

  ringOffset :: Ring -> Int
  ringOffset =
    case _ of
      LeftRing -> 0
      CenterRing -> 18
      RightRing -> 36
