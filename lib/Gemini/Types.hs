module Gemini.Types
  ( -- core types and operations
    Gemini, geminiFromList, geminiIx, ringIndex , initialGemini
  , Location(..), location, isCanonical, isIntersection, sibling
  , Ring(..) , Disk(..) , Color(..), RotationDirection(..), Rotation(..)
  , Move(..), Motion(..), moveCycles
  , toMove, toMotion
  , applyToGemini, applyToHistory
  , ToPermutation(..)
    -- ui types
  , Store(..), HoverState(..), DragState(..), Options(..)
  , Point(..)
    -- re export Seq constructors
  , pattern (:<|), pattern (:|>)
  ) where

import           Relude                 hiding (cycle)

import           Data.Cyclic
import           Data.Finitary
import           Data.Group             (Group (..))
import qualified Data.IntMap            as Map
import qualified Data.List              as List
import qualified Data.List.NonEmpty     as NE
import           Data.Permutation
import           Data.Sequence          (Seq ((:<|), (:|>)))
import qualified Data.Sequence          as Seq
import qualified Data.Text              as Text
import           Optics
import           Optics.State.Operators
import           Prettyprinter          (Pretty (..))
import qualified Prettyprinter          as Pretty
import           System.Random.Stateful (Uniform (..))


-- | UI Definitions
data Store = Store
  { gemini   :: !Gemini
  , history  :: !(Seq Motion)
  , moves    :: !(Seq Move)
  , hover    :: !HoverState
  , drag     :: !(Maybe DragState)
  , options  :: !Options
  , debugLog :: !Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data HoverState = HoverState
  { activeCycle :: !(Maybe (Cycle Location))
  , overMove    :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data DragState = DragState
  { ring         :: !Ring
  , initialAngle :: !Double
  , currentAngle :: !Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


data Options = Options
  { showLabels :: !Bool
  , animate    :: !Bool
  , recording  :: !Bool
  , debug      :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


data Move = Move
  { motions     :: !(Seq Motion)
  , permutation :: !GeminiPermutation
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data Motion = Motion
  { amount   :: !Int
  , rotation :: !Rotation
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Pretty Motion where
  pretty Motion { amount, rotation } =
    pretty amount <> pretty rotation
  prettyList = Pretty.sep . fmap pretty


data Point = Point
  { x :: !Double
  , y :: !Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Pretty Point where
  pretty (Point x y)
    = "("
    <> pretty x
    <> ", "
    <> pretty y
    <> ")"

instance Semigroup Point where
  Point x1 y1 <> Point x2 y2 = Point (x1 + x2) (y1 + y2)

instance Monoid Point where
  mempty = Point 0 0

instance Group Point where
  invert (Point x y) = Point (-x) (-y)

-- | Core Definitions
type GeminiPermutation = Permutation 54

newtype Gemini = Gemini { geminiDiskMap :: IntMap Disk }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)


-- | Location on the gemini puzzle, where a disk can slide
-- the 4 locations where a pair of rings intersect each has two possible representations as a Location type
-- We'll have to normalize somehow.
-- Prefer the leftmost ring?
data Location = Location
  { ring     :: !Ring
  , position :: !(Cyclic 18)
  -- ^ Positions start at the top of the ring, run clockwise from there
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, Finitary)

instance Pretty Location where
  pretty Location { ring, position = Cyclic n} =
    pretty ring <> Pretty.viaShow n

location :: Ring -> Int -> Location
location r p = Location r (cyclic p)

-- | Gemini transformations
-- The 6 basic motions are called:
-- L, L', C, C', R, R'
-- L is a clockwise rotation of the left ring, L' is anticlockwise
-- C is for the central ring
-- R is for the right ring
data Rotation = Rotation
  { ring      :: !Ring
  , direction :: !RotationDirection
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Uniform, Finitary)

instance Pretty Rotation where
  pretty Rotation { ring, direction } = pretty ring <> if direction == Clockwise then "" else "'"

data RotationDirection
  = Clockwise
  | AntiClockwise
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Uniform, Finitary)


data Ring
  = LeftRing
  | CenterRing
  | RightRing
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData, Uniform, Finitary)

instance Pretty Ring where
  pretty LeftRing   = "L"
  pretty CenterRing = "C"
  pretty RightRing  = "R"


data Disk = Disk
  { color :: !Color
  , label :: !Int
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData)

instance Pretty Disk where
  pretty Disk { color, label } =
    pretty color <> Pretty.viaShow label


data Color
  = White
  | Yellow
  | Black
  | Red
  | Green
  | Blue
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData)

instance Pretty Color where
  pretty White  = "W"
  pretty Yellow = "Y"
  pretty Black  = "B"
  pretty Red    = "R"
  pretty Green  = "G"
  pretty Blue   = "U"


-- | Basic operations
class ToPermutation a where
  toPerm :: a -> GeminiPermutation

instance (Foldable f, ToPermutation a) => ToPermutation (f a) where
  toPerm = foldMap toPerm

instance ToPermutation GeminiPermutation where
  toPerm = identity

instance ToPermutation (Cycles Int)  where
  toPerm = fromCycles

instance ToPermutation Rotation where
  toPerm Rotation { ring, direction } =
    fromCycles . cycles . Seq.singleton . cycle $
    flip map positions $ locationToIndex . Location ring
    where
      positions = case direction of
        Clockwise     -> inhabitants
        AntiClockwise -> reverse inhabitants

instance ToPermutation Motion where
  toPerm Motion { amount, rotation } = (`pow` amount) $ toPerm rotation


moveCycles :: Move -> Cycles Location
moveCycles = fmap indexToLocation . toCycles . view #permutation


applyToGemini :: ToPermutation a => a -> Gemini -> Gemini
applyToGemini = permuteGemini . toPerm


permuteGemini :: GeminiPermutation -> Gemini -> Gemini
permuteGemini p (Gemini disks) =
  Gemini $
  flip execState mempty $
    for_ (domain p) $ \n -> do
      let disk = disks ^? ix n
      case disk of
        Nothing   -> pure ()
        Just disk -> at (permute p n) ?= disk



geminiFromList :: [(Location, Disk)] -> Gemini
geminiFromList items = Gemini $ fromList $
  map (\(location, disk) -> (locationToIndex location, disk)) items


locationToIndex :: Location -> Int
locationToIndex = index . canonical
  where
    index Location { ring, position } = (ringIndex ring) * 18 + unCyclic position


-- | if the position exists on two different rings, then prefer the left one
canonical :: Location -> Location
canonical (Location CenterRing 16) = Location LeftRing 2
canonical (Location CenterRing 11) = Location LeftRing 7
canonical (Location RightRing 16)  = Location CenterRing 2
canonical (Location RightRing 11)  = Location CenterRing 7
canonical location                 = location


-- | if the position exists on two different rings, then prefer the right one
inverseCanonical :: Location -> Location
inverseCanonical (Location LeftRing 2)   = Location CenterRing 16
inverseCanonical (Location LeftRing 7)   = Location CenterRing 11
inverseCanonical (Location CenterRing 2) = Location RightRing 16
inverseCanonical (Location CenterRing 7) = Location RightRing 11
inverseCanonical location                = location

isCanonical :: Location -> Bool
isCanonical location = canonical location == location

isIntersection :: Location -> Bool
isIntersection l = canonical l /= l || inverseCanonical l /= l

-- | The other name for this location, if it has one
sibling :: Location -> Maybe Location
sibling l = filter (/= l) [canonical l, inverseCanonical l] ^? ix 0


-- | Invert a disk coordinate to its canonical location
indexToLocation :: Int -> Location
indexToLocation n = Location ring (cyclic p)
  where
    (r, p) = n `quotRem` 18
    ring = case r of
        0 -> LeftRing
        1 -> CenterRing
        _ -> RightRing

geminiIx :: Location -> AffineTraversal' Gemini Disk
geminiIx location = #geminiDiskMap % ix (locationToIndex location)


ringIndex :: Ring -> Int
ringIndex LeftRing   = 0
ringIndex CenterRing = 1
ringIndex RightRing  = 2


initialGemini :: Gemini
initialGemini = geminiFromList
  -- red
  [ (Location LeftRing 12, Disk Red 1)
  , (Location LeftRing 13, Disk Red 2)
  , (Location LeftRing 14, Disk Red 3)
  , (Location LeftRing 15, Disk Red 4)
  , (Location LeftRing 16, Disk Red 5)
  , (Location LeftRing 17, Disk Red 6)
  , (Location LeftRing 0, Disk Red 7)
  , (Location LeftRing 1, Disk Red 8)
  -- yellow
  , (Location LeftRing 3, Disk Yellow 1)
  , (Location LeftRing 4, Disk Yellow 2)
  , (Location LeftRing 5, Disk Yellow 3)
  , (Location LeftRing 6, Disk Yellow 4)
  , (Location LeftRing 7, Disk Yellow 5)
  , (Location LeftRing 8, Disk Yellow 6)
  , (Location LeftRing 9, Disk Yellow 7)
  , (Location LeftRing 10, Disk Yellow 8)
  , (Location LeftRing 11, Disk Yellow 9)
  -- black
  , (Location CenterRing 12, Disk Black 1)
  , (Location CenterRing 13, Disk Black 2)
  , (Location CenterRing 14, Disk Black 3)
  , (Location CenterRing 15, Disk Black 4)
  , (Location CenterRing 16, Disk Black 5)
  , (Location CenterRing 17, Disk Black 6)
  , (Location CenterRing 0, Disk Black 7)
  , (Location CenterRing 1, Disk Black 8)
  -- blue
  , (Location CenterRing 3, Disk Blue 1)
  , (Location CenterRing 4, Disk Blue 2)
  , (Location CenterRing 5, Disk Blue 3)
  , (Location CenterRing 6, Disk Blue 4)
  , (Location CenterRing 7, Disk Blue 5)
  , (Location CenterRing 8, Disk Blue 6)
  , (Location CenterRing 9, Disk Blue 7)
  , (Location CenterRing 10, Disk Blue 8)
  -- green
  , (Location RightRing 3, Disk Green 1)
  , (Location RightRing 4, Disk Green 2)
  , (Location RightRing 5, Disk Green 3)
  , (Location RightRing 6, Disk Green 4)
  , (Location RightRing 7, Disk Green 5)
  , (Location RightRing 8, Disk Green 6)
  , (Location RightRing 9, Disk Green 7)
  , (Location RightRing 10, Disk Green 8)
  -- white
  , (Location RightRing 12, Disk White 1)
  , (Location RightRing 13, Disk White 2)
  , (Location RightRing 14, Disk White 3)
  , (Location RightRing 15, Disk White 4)
  , (Location RightRing 16, Disk White 5)
  , (Location RightRing 17, Disk White 6)
  , (Location RightRing 0, Disk White 7)
  , (Location RightRing 1, Disk White 8)
  , (Location RightRing 2, Disk White 9)
  ]


isSolved :: Gemini -> Bool
isSolved gemini =
  gemini ^. #geminiDiskMap
    & Map.toList
    & map (over _1 indexToLocation)
    & sortBy (compare `on` view _2)
    & List.groupBy ((==) `on` view _2)
    & map (map (view _1))
    & all areConsecutive

areConsecutive :: [Location] -> Bool
areConsecutive locations = (numberOfRings == 1) || (numberOfRings == 2)
  where
    numberOfRings = length ringGroups
    ringGroups = locations & List.groupBy ((==) `on` view #ring)


toMove :: Seq Motion -> Move
toMove motions = Move { motions, permutation = toPerm motions }


applyToHistory :: Motion -> Seq Motion -> Seq Motion
applyToHistory motion Seq.Empty           = fromList [ motion ]
applyToHistory next all@(ms :|> prev) =
  if next ^. #rotation % #ring /= prev ^. #rotation % #ring
  then all :|> next
  else case normalize (combine prev next) of
    Just m -> ms :|> m
    _      -> ms


toMotion :: Rotation -> Motion
toMotion rotation = Motion { amount = 1, rotation }

combine :: Motion -> Motion -> Motion
combine x@(Motion m1 r1) (Motion m2 r2)
  | r1 == r2  = x & #amount %~ (+ m2)
  | otherwise = x & #amount %~ (subtract m2)

opposite :: RotationDirection -> RotationDirection
opposite = \case
  Clockwise     -> AntiClockwise
  AntiClockwise -> Clockwise

normalize :: Motion -> Maybe Motion
normalize Motion { amount = 0 } = Nothing
normalize motion = Just $ do
  let sign = if motion ^. directionL == Clockwise then 1 else -1
  let n = (sign * (motion ^. #amount)) `mod` 18
  if n <= 9
  then motion & #amount .~ n & directionL .~ Clockwise
  else motion & #amount .~ ((-n) `mod` 18) & directionL .~ AntiClockwise

directionL :: Lens' Motion RotationDirection
directionL = #rotation % #direction
