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

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Array.ST as STArray
import Data.Array.ST.Extra as STArray
import Data.Enum (enumFromTo)
import Data.Finitary (inhabitants)
import Data.Foldable (fold, foldMap)
import Data.FoldableWithIndex (foldMapWithIndex, forWithIndex_)
import Data.Location (Location, indexToLocation)
import Data.Location (Ring(..))
import Data.Location (locationToIndex')
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nat (class Pos, knownInt)
import Data.Permutation as Permutation
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
  arbitrary = SolvedGemini <<< permuteInitial <$> ado
    h <- arbitrary <#> if _ then mirrorHorizontal else mempty
    left <- arbitrary <#> if _ then mirrorLeftRing else mempty
    right <- arbitrary <#> if _ then mirrorRightRing else mempty
    colorPerm8 <- arbitrary <#>
      \(AnyPermutation p) -> permuteRanges @4 8 eightDiskRanges p
    colorPerm9 <- arbitrary <#>
      \(AnyPermutation p) -> permuteRanges @2 9 nineDiskRanges p
    in colorPerm8 <> colorPerm9 <> left <> right <> h

newtype ScrambledGemini = ScrambledGemini Gemini

instance Arbitrary ScrambledGemini where
  arbitrary = ado
    AnyPermutation p <- arbitrary
    let perm = toGeminiPermutation p
    in ScrambledGemini $ permuteInitial perm

newtype AlmostSolvedGemini = AlmostSolvedGemini Gemini

instance Arbitrary AlmostSolvedGemini where
  arbitrary = AlmostSolvedGemini <$> do
    (SolvedGemini g) <- arbitrary
    let color l = (geminiLookup l g).color
    (AnyLocation a) <- arbitrary
    (AnyLocation b) <- arbitrary
      `Gen.suchThat` \(AnyLocation b) -> (color a /= color b)
    let i = locationToIndex' a
    let j = locationToIndex' b
    pure $ g # permuteGemini (transpose i j)

permuteInitial :: Permutation 54 -> Gemini
permuteInitial p = permuteGemini p initialGemini

toGeminiPermutation :: Permutation 50 -> Permutation 54
toGeminiPermutation perm = do
  let extended = lift @50 @54 perm
  -- alternates: [52,47,7,2]
  -- | transpose the extra indices to the end of the index space
  let t1 = transpose 47 50
  let t2 = transpose 7 51
  let t3 = transpose 2 53
  let t = t1 <> t2 <> t3
  let conjugated = t <> extended <> t
  conjugated

-- | mirror left ring along a diagonal
-- | in the initial state, this swaps the Red & Yellow bands
mirrorLeftRing :: Permutation 54
mirrorLeftRing =
  fold
    [ transpose 1 3
    , transpose 0 4
    , transpose 17 5
    , transpose 16 6
    , transpose 15 29
    , transpose 14 8
    , transpose 13 9
    , transpose 12 10
    ]

-- | mirror right ring along a diagonal
-- | in the initial state, this swaps the Green & White bands
mirrorRightRing :: Permutation 54
mirrorRightRing =
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

-- | swap every disk along the horizontal axis
mirrorHorizontal :: Permutation 54
mirrorHorizontal = foldMap mirror inhabitants
  where
  mirror :: Ring -> Permutation 54
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

eightDiskRanges :: Array (Array Location)
eightDiskRanges = [ redRange, blackRange, blueRange, greenRange ]

nineDiskRanges :: Array (Array Location)
nineDiskRanges = [ whiteRange, yellowRange ]

permuteRanges ::
  forall @n.
  Pos n =>
  Int ->
  Array (Array Location) ->
  Permutation n ->
  Permutation 54
permuteRanges rangeLen ranges p =
  derangements p
    # foldMapWithIndex \i j ->
        (enumFromTo 0 (rangeLen - 1) :: Array _)
          # foldMap \k ->
              transpose
                (ranges !! i !! k # locationToIndex')
                (ranges !! j !! k # locationToIndex')

infixl 8 unsafeIndex as !!

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex array i =
  case Array.index array i of
    Just x -> x
    _ -> unsafeCrashWith "unsafeIndex"

{-
ranges
  # Array.transpose
  # foldMap
      ( \array -> Permutation.cycle $ permuteArray p
          (array <#> locationToIndex')
      )
      -}

permuteArray :: forall n a. Permutation n -> Array a -> Array a
permuteArray perm array = ST.run do
  copy <- array # STArray.thaw
  forWithIndex_ (derangements perm) $ \i j ->
    case Array.index array i of
      Nothing ->
        unsafeCrashWith $ "index out of range: " <> show i

      Just x ->
        copy # STArray.write j x

  STArray.unsafeFreeze copy

