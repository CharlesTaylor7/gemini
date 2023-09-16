module Data.Gemini
  ( Gemini
  , geminiLookup
  , initialGemini
  , applyToGemini
  , isSolved
  , DiskIndex
  , Location(..)
  , LocationRecord
  , indexToLocation
  , locationToIndex
  , location
  , sibling
  , ambiguousLocations
  , canonical
  , Ring(..)
  , Disk(..)
  , Color(..)
  , Motion(..)
  , GeminiPermutation
  , class ToPermutation
  , toPerm
  , Choice(..)
  , dragRing
  , Chosen(..)
  ) where

import Data.Permutation
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
import Data.Generic.Rep (class Generic)
import Data.Group (pow)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nat (class Pos, D18, D54)
import Data.Semigroup.Foldable as NEFold
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)

-- | An opaque wrapper.
-- Use `geminiFromList` or `initialGemini` to initialize.
-- Use `applyToGemini` to rotate / permute the puzzle.
-- Use `geminiIx` to read / write at specific locations
newtype Gemini = Gemini (Array Disk)

derive instance Generic Gemini _
instance Show Gemini where
  show = genericShow

type DiskIndex = Cyclic D18

-- | Location on the gemini puzzle, where a disk can slide
-- the 4 locations where a pair of rings intersect each has two possible representations as a Location type
-- We normalize by prefering the leftmost ring. See `canonical`.
newtype Location = Location LocationRecord

type LocationRecord =
  { ring :: Ring
  , position :: DiskIndex
  -- ^ Positions start at the top of the ring, run clockwise from there
  }

derive instance Eq Location
derive instance Generic Location _
instance Show Location where
  show = genericShow

unLocation :: Location -> LocationRecord
unLocation (Location r) = r

location :: Ring -> Int -> Location
location ring position = Location { ring, position: cyclic position }

data Ring
  = LeftRing
  | CenterRing
  | RightRing

derive instance Eq Ring
derive instance Ord Ring
derive instance Generic Ring _
instance Show Ring where
  show = genericShow

instance Finitary Ring where
  inhabitants = [ LeftRing, CenterRing, RightRing ]

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
    Permutation
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

type LocationPair =
  { canonical :: Location
  , alternate :: Location
  }

ambiguousLocations :: Array LocationPair
ambiguousLocations =
  [ { canonical: location LeftRing 2, alternate: location CenterRing 16 }
  , { canonical: location LeftRing 7, alternate: location CenterRing 11 }
  , { canonical: location CenterRing 2, alternate: location RightRing 16 }
  , { canonical: location CenterRing 7, alternate: location RightRing 11 }
  ]

-- | if the position exists on two different rings, then prefer the canonical one
canonical :: Location -> Location
canonical location =
  fromMaybe location
    $ Array.find (\{ alternate } -> alternate == location) ambiguousLocations
        <#> _.canonical

-- | if the position exists on two different rings, then prefer the alternate one
alternate :: Location -> Location
alternate location =
  fromMaybe location
    $ Array.find (\{ canonical } -> canonical == location) ambiguousLocations
        <#> _.alternate

-- | The other name for this location, if it has one
sibling :: Location -> Maybe Location
sibling l =
  let
    c = canonical l
    a = alternate l
  in
    (guard (a /= l) *> pure a)
      <|> (guard (c /= l) *> pure c)

siblingIndex :: Int -> Maybe Int
siblingIndex = map locationToIndex' <<< sibling <<< indexToLocation

data Choice a
  = Obvious a
  | Ambiguous a a

data Chosen
  = ChoseLeft
  | ChoseRight

dragRing :: Location -> Choice Location
dragRing loc1 =
  case sibling loc1 of
    Nothing -> Obvious loc1
    Just loc2 -> Ambiguous loc1 loc2

-- | Invert a disk coordinate to its canonical location
indexToLocation :: Int -> Location
indexToLocation n = Location { ring, position: cyclic r }
  where
  (q /\ r) = n `divMod` 18
  ring = case q of
    0 -> LeftRing
    1 -> CenterRing
    2 -> RightRing
    _ -> unsafeCrashWith "indexToLocation"

-- | Convert a location to its index in the gemini map
locationToIndex :: Location -> Int
locationToIndex (Location { ring, position }) =
  (ringIndex ring) * 18 + unCyclic position

  where

  ringIndex :: Ring -> Int
  ringIndex = case _ of
    LeftRing -> 0
    CenterRing -> 1
    RightRing -> 2

locationToIndex' :: Location -> Int
locationToIndex' = canonical >>> locationToIndex

geminiLookup :: Location -> Gemini -> Disk
geminiLookup location (Gemini array) =
  case locationToIndex location # Array.index array of
    Just x -> x
    Nothing -> unsafeCrashWith "geminiLookup"

divMod :: Int -> Int -> Int /\ Int
divMod x y = x `div` y /\ x `mod` y

unsafeGemini :: Array (Location /\ Disk) -> Gemini
unsafeGemini items = Gemini $ ST.run do
  array <- STArray.unsafeNewSized 54
  let
    write loc disk = void $
      array # STArray.modify (locationToIndex loc) (const disk)

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
permuteGemini p (Gemini disks) =
  Gemini $ ST.run do
    array <- disks # STArray.thaw
    for_ (domain p) $ \n -> do
      case Array.index disks n of
        Nothing ->
          unsafeCrashWith "wat"

        Just disk ->
          array # STArray.modify (permute p n) (const disk)

    STArray.unsafeFreeze array

-- | Is the puzzle solved?
-- That is, every disk is grouped with other disks of the same color in sequence.
isSolved :: Gemini -> Boolean
isSolved (Gemini array) =
  array
    # Array.mapWithIndex (/\)
    # Array.groupAllBy (comparing (\(_ /\ disk) -> disk.color))
    # all
        ( \items ->
            let
              { head: _ /\ { color } } = NEArray.uncons items
            in
              items <#> (\(i /\ _) -> indexToLocation i) # isFinishedSequence
                color
        )

-- | Get a location on a specific ring, if it exists on that ring
onRing :: Ring -> Location -> Maybe Location
onRing ring location = check location <|> (check =<< sibling location)
  where
  check loc@(Location location) = do
    guard (location.ring == ring)
    pure loc

-- | check if a set of disk locations is a finished sequence
isFinishedSequence :: Color -> NonEmptyArray Location -> Boolean
isFinishedSequence color ls =
  if numberOfRings > 2 then
    false
  else
    ls
      # map (onRing mostCommonRing)
      # sequence
      # maybe false
          ( isFinished (diskCount color) <<< NEArray.toUnfoldable1 <<< map
              (unLocation >>> _.position)
          )
  where
  diskCount :: Color -> Int
  diskCount White = 9
  diskCount Yellow = 9
  diskCount _ = 8

  mostCommonRing :: Ring
  mostCommonRing =
    ringGroups
      # NEFold.maximumBy (comparing NEArray.length)
      # NEArray.head
      # unLocation
      # _.ring

  ringGroups :: NonEmptyArray (NonEmptyArray Location)
  ringGroups = ls # NEArray.groupAllBy (comparing ((_.ring) <<< unLocation))

  numberOfRings :: Int
  numberOfRings = NEArray.length ringGroups

-- | Linear algorithm to determine if a collection of positions are contiguous when wrapping at the cyclic modulus
isFinished :: forall n. Pos n => Int -> NonEmptyList (Cyclic n) -> Boolean
isFinished expectedCount list =
  let
    { head, tail } = NEList.uncons list
  in
    go tail head head
  where
  precedes :: Cyclic n -> Cyclic n -> Boolean
  precedes a b =
    case compareCyclic a b of
      Precedes -> true
      Equal -> true
      _ -> false

  exceeds :: Cyclic n -> Cyclic n -> Boolean
  exceeds a b =
    case compareCyclic a b of
      Exceeds -> true
      Equal -> true
      _ -> false

  go :: List (Cyclic n) -> Cyclic n -> Cyclic n -> Boolean
  go list min max =
    case List.uncons list of
      Nothing -> true
      Just { head, tail } -> recurse head tail
    where
    recurse x xs
      -- x is the new min
      | x `precedes` min && (unCyclic (max - x) < expectedCount) = go xs x max
      -- x is the new max
      | x `exceeds` max && (unCyclic (x - min) < expectedCount) = go xs min x
      -- x is between the min and max
      | x `precedes` max && x `exceeds` min = go xs min max
      -- x is outside the band of acceptability
      | otherwise = false
