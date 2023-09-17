module Data.Gemini
  ( Gemini
  , disks
  , geminiLookup
  , initialGemini
  , applyToGemini
  , Disk(..)
  , Color(..)
  , Motion(..)
  , GeminiPermutation
  , class ToPermutation
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
import Data.Nat (class Pos, D18, D54)
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

--  Basic operations
type GeminiPermutation = Permutation D54

-- | Typeclass for things that describe permutations of the Gemini puzzle
class ToPermutation a where
  toPerm :: a -> GeminiPermutation

instance ToPermutation Motion where
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
    indices = inhabitants :: Array (Cyclic D18)

instance ToPermutation GeminiPermutation where
  toPerm = identity

newtype Motion = Motion
  { amount :: Cyclic D18
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
initialGemini =
  unsafeGemini
    -- red
    [ (location LeftRing 12 /\ disk Red 1)
    , (location LeftRing 13 /\ disk Red 2)
    , (location LeftRing 14 /\ disk Red 3)
    , (location LeftRing 15 /\ disk Red 4)
    , (location LeftRing 16 /\ disk Red 5)
    , (location LeftRing 17 /\ disk Red 6)
    , (location LeftRing 0 /\ disk Red 7)
    , (location LeftRing 1 /\ disk Red 8)
    -- yellow
    , (location LeftRing 3 /\ disk Yellow 1)
    , (location LeftRing 4 /\ disk Yellow 2)
    , (location LeftRing 5 /\ disk Yellow 3)
    , (location LeftRing 6 /\ disk Yellow 4)
    , (location LeftRing 7 /\ disk Yellow 5)
    , (location LeftRing 8 /\ disk Yellow 6)
    , (location LeftRing 9 /\ disk Yellow 7)
    , (location LeftRing 10 /\ disk Yellow 8)
    , (location LeftRing 11 /\ disk Yellow 9)
    -- black
    , (location CenterRing 12 /\ disk Black 1)
    , (location CenterRing 13 /\ disk Black 2)
    , (location CenterRing 14 /\ disk Black 3)
    , (location CenterRing 15 /\ disk Black 4)
    , (location CenterRing 16 /\ disk Black 5)
    , (location CenterRing 17 /\ disk Black 6)
    , (location CenterRing 0 /\ disk Black 7)
    , (location CenterRing 1 /\ disk Black 8)
    -- blue
    , (location CenterRing 3 /\ disk Blue 1)
    , (location CenterRing 4 /\ disk Blue 2)
    , (location CenterRing 5 /\ disk Blue 3)
    , (location CenterRing 6 /\ disk Blue 4)
    , (location CenterRing 7 /\ disk Blue 5)
    , (location CenterRing 8 /\ disk Blue 6)
    , (location CenterRing 9 /\ disk Blue 7)
    , (location CenterRing 10 /\ disk Blue 8)
    -- green
    , (location RightRing 3 /\ disk Green 1)
    , (location RightRing 4 /\ disk Green 2)
    , (location RightRing 5 /\ disk Green 3)
    , (location RightRing 6 /\ disk Green 4)
    , (location RightRing 7 /\ disk Green 5)
    , (location RightRing 8 /\ disk Green 6)
    , (location RightRing 9 /\ disk Green 7)
    , (location RightRing 10 /\ disk Green 8)
    -- white
    , (location RightRing 12 /\ disk White 1)
    , (location RightRing 13 /\ disk White 2)
    , (location RightRing 14 /\ disk White 3)
    , (location RightRing 15 /\ disk White 4)
    , (location RightRing 16 /\ disk White 5)
    , (location RightRing 17 /\ disk White 6)
    , (location RightRing 0 /\ disk White 7)
    , (location RightRing 1 /\ disk White 8)
    , (location RightRing 2 /\ disk White 9)
    ]

applyToGemini :: forall a. ToPermutation a => a -> Gemini -> Gemini
applyToGemini = permuteGemini <<< toPerm

permuteGemini :: GeminiPermutation -> Gemini -> Gemini
permuteGemini perm (Gemini disks) =
  Gemini $ ST.run do
    array <- disks # STArray.thaw
    forWithIndex_ (derangements perm) $ \i j ->
      case Array.index disks i of
        Nothing ->
          unsafeCrashWith "wat"

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
