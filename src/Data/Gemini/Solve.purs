module Data.Gemini.Solve where

import Data.Location
import Debug
import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Compactable (compact)
import Data.Cyclic (Cyclic, CyclicOrdering(..), compareCyclic, cyclic, unCyclic)
import Data.Foldable (all, any, foldl)
import Data.Gemini (Color(..), Gemini, diskCount, geminiLookup)
import Data.Gemini as Gemini
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Location (Location(..), Ring, ambiguousLocations, indexToLocation, sibling, unLocation)
import Data.Maybe (Maybe(..), maybe)
import Data.Nat (class Pos)
import Data.Semigroup.Foldable as NEFold
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))

-- | Is the puzzle solved?
-- That is, every disk is grouped with other disks of the same color in sequence.
isSolved :: Gemini -> Boolean
isSolved gemini =
  gemini
    # Gemini.disks
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

force :: forall a. (Unit -> a) -> a
force = (#) unit

isSolvedFast :: Gemini -> Boolean
isSolvedFast gemini = all force
  [ \_ -> Set.size (force colors) == 4
  , checkRightmostArc
  , \_ ->
      let
        rings = force junctionRings
      in
        all force
          [ \_ -> Array.length rings == 4
          , \_ -> rings # Array.filter (eq CenterRing) # Array.length # eq 2
          ]
  ]
  where
  -- | checking the junctions + the right ring is enough
  -- | the remaining leftmost arc
  checkRightmostArc :: Unit -> Boolean
  checkRightmostArc _ =
    any force
      [ \_ -> checkFrom (location RightRing 17) (cyclic 1)
      , \_ -> checkFrom (location RightRing 10) (-cyclic 1)
      ]
    where

    checkFrom :: Location -> DiskIndex -> Boolean
    checkFrom start offset =
      1 + go start offset 0 start == diskCount (color start)

  colors :: Unit -> Set Color
  colors _ =
    Set.fromFoldable $
      ambiguousLocations <#>
        \pair -> color pair.canonical

  junctionRings :: Unit -> Array Ring
  junctionRings _ = ambiguousLocations <#> checkJunction # compact

  checkJunction :: LocationPair -> Maybe Ring
  checkJunction { canonical, alternate } =
    [ \_ -> belongsToCompleteArc canonical
    , \_ -> belongsToCompleteArc alternate
    ]
      # map force
      # foldl (<|>) Nothing

  belongsToCompleteArc :: Location -> Maybe Ring
  belongsToCompleteArc start =
    let
      tally = 1 + go start (cyclic 1) 0 start + go start (-cyclic 1) 0 start
      Location { ring } = start
    in
      if tally == diskCount (color start) then Just ring else Nothing

  color :: Location -> Color
  color l = (geminiLookup l gemini).color

  go :: Location -> Cyclic 18 -> Int -> Location -> Int
  go start offset count l =
    let
      next = advance l offset
    in
      if color next == color start then go start offset (count + 1) next
      else count

advance :: Location -> Cyclic 18 -> Location
advance (Location { position, ring }) offset =
  Location { position: position + offset, ring }

