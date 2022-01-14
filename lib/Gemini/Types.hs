module Gemini.Types
  ( -- core types and operations
    Gemini, geminiFromList, geminiIx, ringIndex , initialGemini, rotate
  , Location(..), location
  , Ring(..) , Disk(..) , Color(..), RotationDirection(..), Rotation(..)
  , Move(..), Motion(..)
  , toMove
  , continueMotion
    -- ui types
  , Store(..), Options(..)
    -- re export Seq constructors
  , pattern (:<|), pattern (:|>), pattern EmptySequence
  ) where

import           Relude                 hiding (cycle)

import           Data.Finitary
import qualified Data.IntMap            as Map
import qualified Data.List              as List
import qualified Data.List.NonEmpty     as NE
import           Data.Sequence          (Seq ((:<|), (:|>)))
import qualified Data.Sequence          as Seq
import qualified Data.Text              as Text
import           Optics
import           Optics.State.Operators
import           Permutation
import           Prettyprinter          (Pretty (..))
import qualified Prettyprinter          as Pretty
import           System.Random.Stateful

-- |
pattern EmptySequence :: forall a. Seq a
pattern EmptySequence = Seq.Empty

-- | UI Definitions
data Store = Store
  { gemini  :: !Gemini
  , history :: !(Seq Motion)
  , moves   :: !(Seq Move)
  , options :: !Options
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data Options = Options
  { showLabels :: !Bool
  , animate    :: !Bool
  , useSvg     :: !Bool
  , recording  :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | Definitions
data Move = Move
  { motions     :: !(Seq Motion)
  , permutation :: !GeminiPermutation
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Pretty Move where
  pretty Move { motions, permutation } =
    Pretty.hsep
      [ pretty $ toList $ motions
      , ": "
      , pretty $ fmap indexToLocation $ toCycles $ permutation
      ]

data Motion = Motion
  { amount   :: Int
  , rotation :: Rotation
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Pretty Motion where
  pretty Motion { amount, rotation } =
    pretty amount <> pretty rotation

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
  , position :: !Int
  -- ^ positions run from 0 to 17 inclusive. They start at the topmost position, and run clockwise from their
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)

location :: Ring -> Int -> Location
location r p = Location r (p `mod` 18)

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

instance Pretty Location where
  pretty Location { ring, position } =
    pretty ring <> Pretty.viaShow position

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

rotate :: Rotation -> Gemini -> Gemini
rotate = permuteGemini . rotationToPermutation

permuteGemini :: GeminiPermutation -> Gemini -> Gemini
permuteGemini p (Gemini disks) =
  Gemini $
  flip execState mempty $
    for_ (domain p) $ \n -> do
      let disk = disks ^? ix n
      case disk of
        Nothing   -> pure ()
        Just disk -> at (permute p n) ?= disk


rotationToPermutation :: Rotation -> GeminiPermutation
rotationToPermutation Rotation { ring, direction } =
  fromCycles . cycles . Seq.singleton . cycle $
  flip map positions $ locationToIndex . Location ring
  where
    positions = case direction of
      Clockwise     -> [0..17]
      AntiClockwise -> [17,16..0]
  {--
Rotation { ring = r, direction = d } g = flip execState g $
  for_ disks $ \(i, disk) ->
    case disk of
      Just disk -> geminiIx (Location r i) .= disk
      _         -> pure ()
  where
    step Clockwise     = -1
    step AntiClockwise = 1

    disks :: [(Int, Maybe Disk)]
    disks = flip map [0..17] $ \p ->
      let next = p + step d
      in (p, g ^? geminiIx (location r next))
--}


geminiFromList :: [(Location, Disk)] -> Gemini
geminiFromList items = Gemini $ fromList $
  map (\(location, disk) -> (locationToIndex location, disk)) items


locationToIndex :: Location -> Int
locationToIndex = index . canonical
  where
    index Location { ring, position } = (ringIndex ring) * 18 + position

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


-- | Invert a disk coordinate to its canonical location
indexToLocation :: Int -> Location
indexToLocation n = Location ring p
  where
    (r, p) = n `quotRem` 18
    ring = case r of
        0 -> LeftRing
        1 -> CenterRing
        2 -> RightRing
        _ -> LeftRing

geminiIx :: Location -> AffineTraversal' Gemini Disk
geminiIx location = #geminiDiskMap % ix (locationToIndex location)


ringIndex :: Ring -> Int
ringIndex LeftRing   = 0
ringIndex CenterRing = 1
ringIndex RightRing  = 2


initialGemini :: Gemini
initialGemini = geminiFromList
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
  , (Location LeftRing 10, Disk White 8)
  , (Location LeftRing 11, Disk White 9)
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


toPermutation :: Seq Motion -> GeminiPermutation
toPermutation = foldMap toPerm
  where
    toPerm :: Motion -> GeminiPermutation
    toPerm Motion { amount, rotation } = stimes amount $ rotationToPermutation rotation


toMove :: Seq Motion -> Move
toMove motions = Move { motions, permutation = toPermutation motions }

continueMotion :: Rotation -> Seq Motion -> Seq Motion
continueMotion rotation Seq.Empty         = fromList [ newMotion rotation ]
continueMotion rotation all@(ms :|> motion) =
  if rotation ^. #ring /= motion ^. #rotation % #ring
  then all :|> newMotion rotation
  else case normalize (combine motion rotation) of
    Just m -> ms :|> m
    _      -> ms


newMotion :: Rotation -> Motion
newMotion rotation = Motion { amount = 1, rotation }

combine :: Motion -> Rotation -> Motion
combine m@Motion { rotation } r
  | rotation == r = m & #amount %~ (+ 1)
  | otherwise     = m & #amount %~ (subtract 1)

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
