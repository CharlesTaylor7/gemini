module Gemini.Types
  ( -- core types and operations
    Gemini, geminiFromList, geminiIx, ringIndex , initialGemini, isSolved
  , Location(..), location, isCanonical, isIntersection, sibling, Choice(..), dragRing
  , Ring(..) , Disk(..) , Color(..), RotationDirection(..), Rotation(..)
  , Move(..), Motion(..), moveCycles
  , Point(..), norm
  , toMove, toMotion, normalize
  , applyToGemini, applyToHistory
  , ToPermutation(..)
    -- ui types
  , Store(..), HoverState(..), DragState(..), Options(..), Env(..), Deployment(..), DomInfo(..)
    -- re export Seq constructors
  , pattern (:<|), pattern (:|>)
  , isFinished
  ) where

import           Relude                 hiding (cycle)

import           Data.Angle             as Angle
import           Data.Cyclic            as Cyclic
import           Data.Finitary          as Finitary
import           Data.Permutation       as Permutation
import           Data.Point             as Point

import           Data.Group             (Group (..))
import qualified Data.IntMap            as Map
import qualified Data.List              as List
import qualified Data.List.NonEmpty     as NE
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
  { gemini  :: !Gemini
  , history :: !(Seq Motion)
  , moves   :: !(Seq Move)
  , hover   :: !HoverState
  , drag    :: !(Maybe DragState)
  , options :: !Options
  , env     :: !Env
  , dom     :: !DomInfo
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
  { location     :: !(Choice Location)
  , initialPoint :: !Point
  , currentPoint :: !Point
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Pretty DragState where
  pretty DragState { location, initialPoint, currentPoint }
    = Pretty.sep [pretty location, pretty initialPoint, pretty currentPoint ]


data DomInfo = DomInfo
  { ringCenters :: !(Map Ring Point)
  , ringRadius  :: !Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


data Options = Options
  { showLabels :: !Bool
  , recording  :: !Bool
  , debug      :: !Bool
  , isMobile   :: !Bool
  , confetti   :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


data Env = Env
  { port       :: !Int
  , commit     :: !Text
  , deployment :: !Deployment
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


data Deployment
  = Prod
  | Dev
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | Core Definitions
data Move = Move
  { motions    :: !(Seq Motion)
  , moveCycles :: !(Cycles Location)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data Motion = Motion
  { amount   :: !(Cyclic 18)
  , rotation :: !Rotation
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Pretty Motion where
  pretty Motion { amount = Cyclic amount, rotation } =
    pretty amount <> pretty rotation
  prettyList = Pretty.sep . fmap pretty

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


--  Basic operations
--
-- | Typeclass for things that describe permutations of the Gemini puzzle
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
  toPerm Motion { amount = Cyclic amount, rotation } = (`pow` amount) $ toPerm rotation



locationCycles :: GeminiPermutation -> Cycles Location
locationCycles = fmap indexToLocation . toCycles


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

-- | Get a location on a specific ring, if it has a name on that ring
onRing :: Location -> Ring -> Maybe Location
onRing (Location CenterRing 16) LeftRing  = Just $ Location LeftRing 2
onRing (Location LeftRing 2) CenterRing   = Just $ Location CenterRing 16

onRing (Location CenterRing 11) LeftRing  = Just $ Location LeftRing 7
onRing (Location LeftRing 7) CenterRing   = Just $ Location CenterRing 11

onRing (Location RightRing 16) CenterRing = Just $ Location CenterRing 2
onRing (Location CenterRing 2) RightRing  = Just $ Location RightRing 16

onRing (Location RightRing 11) CenterRing = Just $ Location CenterRing 7
onRing (Location CenterRing 7) RightRing  = Just $ Location RightRing 11

onRing l@(Location source _) target
  | source == target = Just l
  | otherwise        = Nothing


data Choice a = Obvious a | Ambiguous a a
  deriving stock (Eq, Generic, Show, Functor)
  deriving anyclass (NFData)

instance Pretty a => Pretty (Choice a) where
  pretty = Pretty.viaShow . fmap pretty

dragRing :: Location -> Choice Location
dragRing loc1 =
  case sibling loc1 of
    Nothing   -> Obvious $ loc1
    Just loc2 -> Ambiguous loc1 loc2


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

diskCount :: Color -> Int
diskCount White  = 9
diskCount Yellow = 9
diskCount _      = 8

isSolved :: Gemini -> Bool
isSolved gemini =
  gemini ^. #geminiDiskMap
    & Map.toList
    & selectGroups (view $ _2 % #color)
    & all (\(color, items) -> items <&> (view $ _1 % to indexToLocation) & isFinishedSequence color)


selectGroups :: (Ord key) => (a -> key) -> [a] -> [(key, NonEmpty a)]
selectGroups projection foldable =
  foldable
  & fmap (\x -> (projection x, x))
  & List.sortBy (compare `on` fst)
  & NE.groupBy ((==) `on` fst)
  & fmap (\group -> let (key, _):|_ = group in (key, fmap snd group))



isFinishedSequence :: Color -> NonEmpty Location -> Bool
isFinishedSequence color ls =
  if numberOfRings > 2
  then False
  else ls
    & fmap (`onRing` mostCommonRing)
    & sequenceA
    & maybe False (isFinished (diskCount color) . fmap (view #position))
  where
    mostCommonRing :: Ring
    mostCommonRing = ringGroups & List.maximumBy (compare `on` length) & head & view #ring

    ringGroups :: [NonEmpty Location]
    ringGroups = ls & NE.groupBy ((==) `on` view #ring)

    numberOfRings :: Int
    numberOfRings = length ls


-- | Linear algorithm to determine if a collection of positions are contiguous when wrapping at the cyclic modulus
isFinished :: forall n. (KnownNat n) => Int -> NonEmpty (Cyclic n) -> Bool
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
      | x `precedes` min && (max - x < cyclic expectedCount) = go xs x max
      -- ^ x is the new min
      | x `exceeds` max && (x - min < cyclic expectedCount) = go xs min x
      -- ^ x is the new max
      | x `precedes` max && x `exceeds` min = go xs min max
      -- ^ x is between the min & max
      | otherwise = False
      -- ^ x is outside the band of acceptability


toMove :: Seq Motion -> Move
toMove motions = Move { motions, moveCycles = locationCycles $ toPerm motions }


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

-- | Get the opposite rotation direction
opposite :: RotationDirection -> RotationDirection
opposite = \case
  Clockwise     -> AntiClockwise
  AntiClockwise -> Clockwise


normalize :: Motion -> Maybe Motion
normalize Motion { amount = 0 } = Nothing
normalize motion = Just $ do
  let sign = if motion ^. directionL == Clockwise then 1 else -1
  let n = sign * (motion ^. #amount)
  if n <= 9
  then motion & #amount .~ n & directionL .~ Clockwise
  else motion & #amount .~ (-n) & directionL .~ AntiClockwise
  where
    directionL :: Lens' Motion RotationDirection
    directionL = #rotation % #direction
