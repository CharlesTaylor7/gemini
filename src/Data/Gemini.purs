module Data.Gemini
  ( Gemini
  , disks
  , geminiLookup
  , initialGemini
  , applyToGemini
  , permuteGemini
  , diskCount
  , Disk(..)
  , Color(..)
  , Motion(..)
  , GeminiPermutation
  , toPerm
  ) where

import Data.Location
import Debug
import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Array.ST as STArray
import Data.Array.ST.Extra as STArray
import Data.Cyclic (Cyclic, CyclicOrdering(..), compareCyclic, cyclic, unCyclic)
import Data.Enum (class Enum)
import Data.Enum (enumFromTo)
import Data.Finitary (class Finitary, inhabitants)
import Data.Foldable (class Foldable, all, foldMap, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nat (class Pos)
import Data.Permutation (Permutation, derangements, permute, unsafePermutation)
import Data.Semigroup.Foldable as NEFold
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)

-- | An opaque wrapper.
-- Use `unsafeGemini` or `initialGemini` to initialize.
-- Use `applyToGemini` to permute the puzzle.
-- Use `geminiLookup` to read at specific locations.
-- Use `disks` to get the underlying representation as an array.
newtype Gemini = Gemini (Array Disk)

derive instance Generic Gemini _
instance Show Gemini where
  show = genericShow

-- | Get an array of the disks in location order
disks :: Gemini -> Array Disk
disks (Gemini disks) = disks

type Disk =
  { color :: Color
  , label :: Int
  }

disk :: Color -> Int -> Disk
disk color label = { color, label }

data Color
  = White
  | Yellow
  | Black
  | Red
  | Green
  | Blue

derive instance Generic Color _
derive instance Eq Color
derive instance Ord Color
instance Show Color where
  show = genericShow

instance Finitary Color where
  inhabitants = [ Red, Green, Blue, Black, White, Yellow ]

--  Basic operations
type GeminiPermutation = Permutation 54

toPerm :: Motion -> GeminiPermutation
toPerm (Motion { ring, amount }) =
  unsafePermutation
    $ Map.fromFoldable
    $ indices
        <#> \i ->
          let
            start = location ring $ unCyclic i
            end = location ring $ unCyclic $ i + amount
          in
            locationToIndex' start /\ locationToIndex' end
  where
  indices = inhabitants :: Array (Cyclic 18)

newtype Motion = Motion
  { amount :: Cyclic 18
  , ring :: Ring
  }

geminiLookup :: Location -> Gemini -> Disk
geminiLookup location (Gemini array) =
  case locationToIndex location # Array.index array of
    Just x -> x
    Nothing -> unsafeCrashWith "geminiLookup"

unsafeGemini :: Array (Location /\ Disk) -> Gemini
unsafeGemini items = Gemini $ ST.run do
  array <- STArray.unsafeNewSized 54
  let write loc disk = array # STArray.write (locationToIndex loc) disk

  for_ items $ \(location /\ disk) -> do
    write location disk
    case sibling location of
      Nothing ->
        pure unit
      Just s ->
        write s disk

  STArray.unsafeFreeze array

initialGemini :: Gemini
initialGemini = unsafeGemini $
  inhabitants >>= \color ->
    Array.zipWith
      (\l i -> l /\ disk color i)
      (range color)
      (enumFromTo 1 (diskCount color))

diskCount :: Color -> Int
diskCount White = 9
diskCount Yellow = 9
diskCount _ = 8

range :: Color -> Array Location
range =
  case _ of
    Red -> redRange
    Yellow -> yellowRange
    Black -> blackRange
    Blue -> blueRange
    Green -> greenRange
    White -> whiteRange

redRange :: Array Location
redRange =
  [ (location LeftRing 12)
  , (location LeftRing 13)
  , (location LeftRing 14)
  , (location LeftRing 15)
  , (location LeftRing 16)
  , (location LeftRing 17)
  , (location LeftRing 0)
  , (location LeftRing 1)
  ]

yellowRange :: Array Location
yellowRange =
  [ (location LeftRing 3)
  , (location LeftRing 4)
  , (location LeftRing 5)
  , (location LeftRing 6)
  , (location LeftRing 7)
  , (location LeftRing 8)
  , (location LeftRing 9)
  , (location LeftRing 10)
  , (location LeftRing 11)
  ]

blackRange :: Array Location
blackRange =
  [ (location CenterRing 12)
  , (location CenterRing 13)
  , (location CenterRing 14)
  , (location CenterRing 15)
  , (location CenterRing 16)
  , (location CenterRing 17)
  , (location CenterRing 0)
  , (location CenterRing 1)
  ]

blueRange :: Array Location
blueRange =
  [ (location CenterRing 3)
  , (location CenterRing 4)
  , (location CenterRing 5)
  , (location CenterRing 6)
  , (location CenterRing 7)
  , (location CenterRing 8)
  , (location CenterRing 9)
  , (location CenterRing 10)
  ]

greenRange :: Array Location
greenRange =
  [ (location RightRing 3)
  , (location RightRing 4)
  , (location RightRing 5)
  , (location RightRing 6)
  , (location RightRing 7)
  , (location RightRing 8)
  , (location RightRing 9)
  , (location RightRing 10)
  ]

whiteRange :: Array Location
whiteRange =
  [ (location RightRing 12)
  , (location RightRing 13)
  , (location RightRing 14)
  , (location RightRing 15)
  , (location RightRing 16)
  , (location RightRing 17)
  , (location RightRing 0)
  , (location RightRing 1)
  , (location RightRing 2)
  ]

applyToGemini :: Motion -> Gemini -> Gemini
applyToGemini = permuteGemini <<< toPerm

permuteGemini :: GeminiPermutation -> Gemini -> Gemini
permuteGemini perm (Gemini disks) =
  Gemini $ ST.run do
    array <- disks # STArray.thaw
    forWithIndex_ (derangements perm) $ \i j ->
      case Array.index disks i of
        Nothing ->
          unsafeCrashWith $ "index out of range: " <> show i

        Just disk ->
          array # STArray.write j disk

    for_ ambiguousLocations $ \{ canonical, alternate } -> do
      val <- array # STArray.peek (locationToIndex canonical)
      case val of
        Nothing ->
          unsafeCrashWith "wat"
        Just disk ->
          array # STArray.write (locationToIndex alternate) disk

    STArray.unsafeFreeze array
