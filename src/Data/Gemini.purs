module Data.Gemini
  ( Gemini
  , geminiLookup
  , initialGemini
  , applyToGemini
  -- , isSolved, solvedColors
  -- , disksOf , disksFrom
  , DiskIndex
  , Location(..)
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
  -- , normalize
  , GeminiPermutation
  , class ToPermutation
  , toPerm
  , Choice(..)
  , dragRing
  , Chosen(..)
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Cyclic
import Data.Group (pow)
import Data.Enum (class Enum)
import Data.Finitary
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Permutation
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (class Foldable, foldMap)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.List as List
import Data.List.NonEmpty as NE
import Data.Nat
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Type.Equality as TE
import Safe.Coerce (coerce)

-- | An opaque wrapper.
-- Use `geminiFromList` or `initialGemini` to initialize.
-- Use `applyToGemini` to rotate / permute the puzzle.
-- Use `geminiIx` to read / write at specific locations
newtype Gemini
  = Gemini (Map Int Disk)

type DiskIndex
  = Cyclic D18

-- | Location on the gemini puzzle, where a disk can slide
-- the 4 locations where a pair of rings intersect each has two possible representations as a Location type
-- We normalize by prefering the leftmost ring. See `canonical`.
newtype Location
  = Location
  { ring :: Ring
  , position :: DiskIndex
  -- ^ Positions start at the top of the ring, run clockwise from there
  }
derive instance Eq Location

location :: Ring -> Int -> Location
location ring position = Location { ring, position: cyclic position }

data Ring
  = LeftRing
  | CenterRing
  | RightRing
derive instance Eq Ring

instance Finitary Ring where
  inhabitants = [ LeftRing, CenterRing, RightRing ]

type Disk
  = { color :: Color
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
instance Show Color where
  show = genericShow

--  Basic operations
type GeminiPermutation
  = Permutation D54

-- | Typeclass for things that describe permutations of the Gemini puzzle
class ToPermutation a where
  toPerm :: a -> GeminiPermutation

instance ToPermutation Motion where
  toPerm (Motion {}) = unsafeCrashWith ""
--(toPerm rotation) `pow` unCyclic amount
{-
instance ToPermutation Rotation where
  toPerm (Rotation { ring, direction }) =
    fromCycles
      <<< cycles
      <<< Array.singleton
      <<< cycle
      $ flip map positions
      $ locationToIndex'
      <<< Location
      <<< ({ ring, position: _ })
    where
    positions :: Array (Cyclic D18)
    positions = case direction of
      Clockwise -> inhabitants
      AntiClockwise -> Array.reverse inhabitants
      -}
instance ToPermutation GeminiPermutation where
  toPerm = identity

newtype Motion
  = Motion
  { amount :: Cyclic D18
  , ring :: Ring
  }

type LocationPair
  = { canonical :: Location
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
alternate location = location

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

-- | Index into the gemini map
geminiLookup :: Location -> Gemini -> Disk
geminiLookup location (Gemini diskMap) =
  diskMap
    # Map.lookup (locationToIndex' location)
    # unsafeFromJust

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust m = unsafePartial (fromJust m)

divMod :: Int -> Int -> Int /\ Int
divMod x y = x `div` y /\ x `mod` y

unsafeGemini :: forall f. Functor f => Foldable f => f (Location /\ Disk) -> Gemini
unsafeGemini items =
  Gemini
    $ Map.fromFoldable
    $ map (\(location /\ disk) -> (locationToIndex' location /\ disk)) items

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
  Gemini $ Map.fromFoldable items
  where
  lookup = flip Map.lookup disks

  items :: Array (Int /\ Disk)
  items =
    (domain p)
      <#> \n -> do
          case lookup n <|> (siblingIndex n >>= lookup) of
            Nothing -> unsafeCrashWith "wat"
            Just disk -> permute p n /\ disk

{-
disksOf :: Ring -> IxFold DiskIndex Gemini Disk
disksOf ring =
  reindexed Cyclic $
  ifolding $ \g ->
  inhabitants <#> \cyclic -> g ^. geminiIx (Location ring cyclic)


disksFrom :: Ring -> Array DiskIndex -> Fold Gemini (DiskIndex /\ Disk)
disksFrom ring range =
  folding $ \g ->
  range <#> \i -> (i /\ geminiLookup (Location ring i) g)

-- | Get a location on a specific ring, if it exists on that ring
onRing :: Ring -> Location -> Maybe Location
onRing ring location = candidates
  # filter (\loc -> loc.ring == ring)
  # preview (ix 0)
  where
    candidates = location : toList (sibling location)
-- | Is the puzzle solved?
-- That is, every disk is grouped with other disks of the same color in sequence<<<
isSolved :: Gemini -> Bool
isSolved (Gemini diskMap) =
  diskMap
    # Map.toList
    # selectGroups (\(_ /\ disk) -> disk.color)
    # all (\(color /\ items) -> items <#> (\(i/\_) -> indexToLocation i) # isFinishedSequence color)


solvedColors :: Gemini -> Array Color
solvedColors (Gemini diskMap) =
  diskMap
    # Map.toList
    # selectGroups (\(_ /\ disk) -> disk.color)
    # filter (\(color /\ items) -> items <#> (\(i/\_) -> indexToLocation i) # isFinishedSequence color)
    # map fst

toArray :: forall f. FoldableWithIndex f -> f ~> Array
toArray = error ""


-- | Sort # group items by key projection
selectGroups :: forall key a. (Ord key) => (a -> key) -> Array a -> Array (key /\ NonEmpty a)
selectGroups projection foldable =
  foldable
  -- | decorate with the key for comparison
  # fmap (\x -> (projection x /\ x))
  # List.sortBy (compare `on` fst)
  # NE.groupBy ((==) `on` fst)
  -- | only list the key once per group
  # fmap (\group -> let (key, _):|_ = group in (key, fmap snd group))


-- | check if a set of disk locations is a finished sequence
isFinishedSequence :: Color -> NonEmpty Location -> Bool
isFinishedSequence color ls =
  if numberOfRings > 2
  then False
  else ls
    # fmap (onRing mostCommonRing)
    # sequenceA
    # maybe False (isFinished (diskCount color) <<< fmap (_.position))
  where
    diskCount :: Color -> Int
    diskCount White  = 9
    diskCount Yellow = 9
    diskCount _      = 8

    mostCommonRing :: Ring
    mostCommonRing = ringGroups # List.maximumBy (compare `on` length) # head # (_.ring)

    ringGroups :: [NonEmpty Location]
    ringGroups = ls # NE.groupBy ((==) `on` view #ring)

    numberOfRings :: Int
    numberOfRings = length ringGroups


-- | Linear algorithm to determine if a collection of positions are contiguous when wrapping at the cyclic modulus
isFinished :: forall n. Pos n => Int -> NonEmpty (Cyclic n) -> Bool
isFinished expectedCount (head :| rest) = go rest head head
  where
    precedes :: Cyclic n -> Cyclic n -> Bool
    a `precedes` b =
      case compareCyclic a b of
        Precedes -> True
        Equal    -> True
        _        -> False

    exceeds :: Cyclic n -> Cyclic n -> Bool
    a `exceeds` b =
      case compareCyclic a b of
        Exceeds -> True
        Equal   -> True
        _       -> False

    go :: [Cyclic n] -> Cyclic n -> Cyclic n -> Bool
    go [] _   _   = True
    go (x:xs) min max
      | x `precedes` min ## (max - x < Cyclic expectedCount) = go xs x max
      -- ^ x is the new min
      | x `exceeds` max ## (x - min < Cyclic expectedCount) = go xs min x
      -- ^ x is the new max
      | x `precedes` max ## x `exceeds` min = go xs min max
      -- ^ x is between the min # max
      | otherwise = False
      -- ^ x is outside the band of acceptability
-}
