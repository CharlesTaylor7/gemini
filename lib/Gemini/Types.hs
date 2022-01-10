module Gemini.Types
  ( Gemini, geminiFromList
  , Location(..)
  , Ring(..)
  , Disk(..)
  , Color(..)
  , solvedGemini
  ) where

import           Relude

import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import           Prettyprinter.Render.Text

import           Optics

import qualified Data.Map                  as Map
import qualified Data.Text                 as Text


-- | Definitions
newtype Gemini = Gemini (IntMap Disk)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)


-- | Location on the gemini puzzle, where a disk can slide
-- the 4 locations where a pair of rings intersect each has two possible representations as a Location type
-- We'll have to normalize somehow.
-- Prefer the leftmost ring?
data Location = Location
  { ring     :: !Ring
  , position :: !Int
  -- ^ positions run from 0 to 17 inclusive. They start at the topmost position, and run clockwise from their
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)


instance Pretty Location where
  pretty Location { ring, position } =
    pretty ring <> Pretty.unsafeViaShow position


data Ring
  = LeftRing
  | CenterRing
  | RightRing
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData)

instance Pretty Ring where
  pretty LeftRing   = "L"
  pretty CenterRing = "C"
  pretty RightRing  = "R"


data Disk = Disk
  { color :: !Color
  , label :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Pretty Disk where
  pretty Disk { color, label } =
    pretty color <> Pretty.unsafeViaShow label


data Color
  = White
  | Yellow
  | Black
  | Red
  | Green
  | Blue
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Pretty Color where
  pretty White  = "W"
  pretty Yellow = "Y"
  pretty Black  = "B"
  pretty Red    = "R"
  pretty Green  = "G"
  pretty Blue   = "U"


-- | Basic operations

-- | lookup location on gemini puzzle
geminiLookup :: Gemini -> Location -> Disk
geminiLookup (Gemini map) location = map ^? ix index & fromMaybe (error "impossible")
  where
    index = locationToIndex location

geminiFromList :: [(Location, Disk)] -> Gemini
geminiFromList items = Gemini $ fromList $
  map (\(location, disk) -> (locationToIndex location, disk)) items


locationToIndex :: Location -> Int
locationToIndex = index . canonical
  where
    index Location { ring, position } = (ringIndex ring) * 18 + position

    -- | identity if the position only has 1 possible description
    -- if the position exists on two different rings, then prefer the left one
    canonical :: Location -> Location
    canonical (Location CenterRing 16) = Location LeftRing 2
    canonical (Location CenterRing 16) = Location LeftRing 7
    canonical (Location RightRing 16)  = Location CenterRing 2
    canonical (Location RightRing 11)  = Location CenterRing 7
    canonical location                 = location



ringIndex :: Ring -> Int
ringIndex LeftRing   = 0
ringIndex CenterRing = 1
ringIndex RightRing  = 2


allDisks :: [Disk]
allDisks
  =  diskSet White 9
  <> diskSet Yellow 9
  <> diskSet Black 8
  <> diskSet Red 8
  <> diskSet Green 8
  <> diskSet Blue 8
  where
    diskSet :: Color -> Int -> [Disk]
    diskSet color n = map (Disk color) [1..n]


solvedGemini :: Gemini
solvedGemini = geminiFromList
  -- blue
  [ (Location LeftRing 12, Disk Blue 1)
  , (Location LeftRing 13, Disk Blue 2)
  , (Location LeftRing 14, Disk Blue 3)
  , (Location LeftRing 15, Disk Blue 4)
  , (Location LeftRing 16, Disk Blue 5)
  , (Location LeftRing 17, Disk Blue 6)
  , (Location LeftRing 0, Disk Blue 7)
  , (Location LeftRing 1, Disk Blue 8)
  -- white
  , (Location LeftRing 3, Disk White 1)
  , (Location LeftRing 4, Disk White 2)
  , (Location LeftRing 5, Disk White 3)
  , (Location LeftRing 6, Disk White 4)
  , (Location LeftRing 7, Disk White 5)
  , (Location LeftRing 8, Disk White 6)
  , (Location LeftRing 9, Disk White 7)
  -- green
  , (Location CenterRing 12, Disk Green 1)
  , (Location CenterRing 13, Disk Green 2)
  , (Location CenterRing 14, Disk Green 3)
  , (Location CenterRing 15, Disk Green 4)
  , (Location CenterRing 16, Disk Green 5)
  , (Location CenterRing 17, Disk Green 6)
  , (Location CenterRing 0, Disk Green 7)
  , (Location CenterRing 1, Disk Green 8)
  -- red
  , (Location CenterRing 3, Disk Red 1)
  , (Location CenterRing 4, Disk Red 2)
  , (Location CenterRing 5, Disk Red 3)
  , (Location CenterRing 6, Disk Red 4)
  , (Location CenterRing 7, Disk Red 5)
  , (Location CenterRing 8, Disk Red 6)
  , (Location CenterRing 9, Disk Red 7)
  , (Location CenterRing 10, Disk Red 8)
  -- black
  , (Location RightRing 3, Disk Black 1)
  , (Location RightRing 4, Disk Black 2)
  , (Location RightRing 5, Disk Black 3)
  , (Location RightRing 6, Disk Black 4)
  , (Location RightRing 7, Disk Black 5)
  , (Location RightRing 8, Disk Black 6)
  , (Location RightRing 9, Disk Black 7)
  , (Location RightRing 10, Disk Black 8)
  -- yellow
  , (Location RightRing 12, Disk Yellow 1)
  , (Location RightRing 13, Disk Yellow 2)
  , (Location RightRing 14, Disk Yellow 3)
  , (Location RightRing 15, Disk Yellow 4)
  , (Location RightRing 16, Disk Yellow 5)
  , (Location RightRing 17, Disk Yellow 6)
  , (Location RightRing 0, Disk Yellow 7)
  , (Location RightRing 1, Disk Yellow 8)
  , (Location RightRing 2, Disk Yellow 9)
  ]
