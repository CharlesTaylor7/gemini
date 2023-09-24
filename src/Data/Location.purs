module Data.Location where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array as Array
import Data.Cyclic (Cyclic, cyclic, unCyclic)
import Data.Finitary (class Finitary)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafeCrashWith)

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

instance Ord Location where
  compare = comparing locationToIndex

instance Show Location where
  show = genericShow

unLocation :: Location -> LocationRecord
unLocation (Location r) = r

location :: Ring -> Int -> Location
location ring position = Location { ring, position: cyclic position }

type DiskIndex = Cyclic 18

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

type LocationPair =
  { canonical :: Location
  , alternate :: Location
  }

ambiguousLocations :: Array LocationPair
ambiguousLocations =
  [ { canonical: location CenterRing 2, alternate: location RightRing 16 }
  , { canonical: location CenterRing 7, alternate: location RightRing 11 }
  , { canonical: location CenterRing 11, alternate: location LeftRing 7 }
  , { canonical: location CenterRing 16, alternate: location LeftRing 2 }
  ]

-- | if the position exists on two different rings, then prefer the canonical one
canonical :: Location -> Location
canonical loc@(Location l) | l.ring == CenterRing = loc
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

-- | Invert a disk coordinate to its canonical location
indexToLocation :: Int -> Location
indexToLocation n = Location { ring, position: cyclic r }
  where
  q = n `div` 18
  r = n `mod` 18

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
