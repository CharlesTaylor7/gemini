module Data.Gemini
  ( Gemini, applyToGemini, isSolved, geminiIx, initialGemini, solvedColors
  , disksOf
  , look
  -- | Positions
  , pattern L, pattern C, pattern R
  -- | Rotations
  , l, c, r
  , Location(..), indexToLocation, sibling, ambiguousLocations, canonical
  , Ring(..), Rotation(..), RotationDirection(..), Disk(..), Color(..)
  , Motion(..), normalize
  , GeminiPermutation
  , ToPermutation(..)
  , Choice(..), dragRing, Chosen(..)
  )
  where

import           Relude                 hiding (cycle, get)

import           Optics
import           Optics.State.Operators

import           Data.Cyclic
import           Data.Finitary
import           Data.Permutation

import qualified Data.IntMap            as Map
import qualified Data.List              as List
import qualified Data.List.NonEmpty     as NE
import qualified Data.Sequence          as Seq
import qualified Prettyprinter          as Pretty

import           Data.Aeson             (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Prettyprinter          (Pretty (..))
import           System.Random.Stateful (Uniform (..))


-- | An opaque wrapper.
-- Use `geminiFromList` or `initialGemini` to initialize.
-- Use `applyToGemini` to rotate / permute the puzzle.
-- Use `geminiIx` to read / write at specific locations
newtype Gemini = Gemini { geminiDiskMap :: IntMap Disk }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

instance Each Location Gemini Gemini Disk Disk where
  each = reindexed indexToLocation $ #geminiDiskMap % each


type DiskIndex = Cyclic 18

-- | Location on the gemini puzzle, where a disk can slide
-- the 4 locations where a pair of rings intersect each has two possible representations as a Location type
-- We normalize by prefering the leftmost ring. See `canonical`.
data Location = Location
  { ring     :: !Ring
  , position :: !DiskIndex
  -- ^ Positions start at the top of the ring, run clockwise from there
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

instance Finitary Location where
  inhabitants = Location <$> inhabitants <*> inhabitants

instance Pretty Location where
  pretty Location { ring, position = Cyclic n} =
    pretty ring <> Pretty.viaShow n


-- | Positions
{-# COMPLETE L, C, R #-}
pattern L, C, R :: DiskIndex -> Location
pattern L n = Location LeftRing n
pattern C n = Location CenterRing n
pattern R n = Location RightRing n


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
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData, Uniform)

instance Finitary Rotation where
  inhabitants = Rotation <$> inhabitants <*> inhabitants

instance Pretty Rotation where
  pretty Rotation { ring, direction } = pretty ring <> if direction == Clockwise then "" else "'"

data RotationDirection
  = Clockwise
  | AntiClockwise
  deriving stock (Eq, Show, Generic, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData, Uniform, Finitary)


data Ring
  = LeftRing
  | CenterRing
  | RightRing
  deriving stock (Eq, Show, Generic, Ord, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
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
  deriving anyclass (FromJSON, ToJSON)
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
  deriving stock (Eq, Show, Generic, Ord, Bounded, Enum)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData, Finitary)

instance Pretty Color where
  pretty White  = "W"
  pretty Yellow = "Y"
  pretty Black  = "B"
  pretty Red    = "R"
  pretty Green  = "G"
  pretty Blue   = "U"


--  Basic operations
type GeminiPermutation = Permutation 54


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


data Motion = Motion
  { amount   :: !(Cyclic 18)
  , rotation :: !Rotation
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

instance ToPermutation Motion where
  toPerm Motion { amount = Cyclic amount, rotation } = (`pow` amount) $ toPerm rotation

instance Pretty Motion where
  pretty Motion { amount = Cyclic amount, rotation } =
    pretty amount <> pretty rotation
  prettyList = Pretty.sep . fmap pretty


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


-- | Motions
l, c, r :: Cyclic 18 -> Motion
l n = Motion { amount = n, rotation = Rotation { direction = Clockwise, ring = LeftRing }}
c n = Motion { amount = n, rotation = Rotation { direction = Clockwise, ring = CenterRing }}
r n = Motion { amount = n, rotation = Rotation { direction = Clockwise, ring = RightRing }}




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

ambiguousLocations :: [(Location, Location)]
ambiguousLocations =
  [ (Location LeftRing 2  , Location CenterRing 16)
  , (Location LeftRing 7  , Location CenterRing 11)
  , (Location CenterRing 2, Location RightRing  16)
  , (Location CenterRing 7, Location RightRing  11)
  ]

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


-- | The other name for this location, if it has one
sibling :: Location -> Maybe Location
sibling l = [canonical l, inverseCanonical l]
  & filter (/= l)
  & preview (ix 0)


-- | Get a location on a specific ring, if it exists on that ring
onRing :: Ring -> Location -> Maybe Location
onRing ring location = candidates
  & filter (\loc -> loc ^. #ring == ring)
  & preview (ix 0)
  where
    candidates = location : toList (sibling location)


data Choice a = Obvious a | Ambiguous a a
  deriving stock (Eq, Generic, Show, Functor)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

instance Pretty a => Pretty (Choice a) where
  pretty = Pretty.viaShow . fmap pretty

data Chosen
  = ChoseLeft
  | ChoseRight
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

dragRing :: Location -> Choice Location
dragRing loc1 =
  case sibling loc1 of
    Nothing   -> Obvious loc1
    Just loc2 -> Ambiguous loc1 loc2


-- | Invert a disk coordinate to its canonical location
indexToLocation :: Int -> Location
indexToLocation n = Location ring (Cyclic p)
  where
    (r, p) = n `quotRem` 18
    ring = case r of
        0 -> LeftRing
        1 -> CenterRing
        _ -> RightRing


-- | Convert a location to its index in the gemini map
locationToIndex :: Location -> Int
locationToIndex = index . canonical
  where
    index Location { ring, position } = (ringIndex ring) * 18 + unCyclic position

    ringIndex :: Ring -> Int
    ringIndex = \case
      LeftRing   -> 0
      CenterRing -> 1
      RightRing  -> 2


-- | Index into the gemini map
geminiIx :: Location -> Lens' Gemini Disk
geminiIx location = #geminiDiskMap % trustMe (ix (locationToIndex location))
  where
    trustMe :: forall s a. AffineTraversal' s a -> Lens' s a
    trustMe t = lens (fromMaybe (error "trustMe") . preview t) (flip $ set t)


look :: Location -> Gemini -> Disk
look location g = fromMaybe (error "invalid gemini") $
  g ^? #geminiDiskMap % ix (locationToIndex location)


disksOf :: Ring -> IxFold DiskIndex Gemini Disk
disksOf ring =
  reindexed Cyclic $
  ifolding $ \g ->
  inhabitants <&> \cyclic -> g ^. geminiIx (Location ring cyclic)


geminiFromList :: [(Location, Disk)] -> Gemini
geminiFromList items = Gemini $ fromList $
  map (\(location, disk) -> (locationToIndex location, disk)) items


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


-- | Is the puzzle solved?
-- That is, every disk is grouped with other disks of the same color in sequence.
isSolved :: Gemini -> Bool
isSolved gemini =
  gemini ^. #geminiDiskMap
    & Map.toList
    & selectGroups (view $ _2 % #color)
    & all (\(color, items) -> items <&> (view $ _1 % to indexToLocation) & isFinishedSequence color)


solvedColors :: Gemini -> [Color]
solvedColors gemini =
  gemini ^. #geminiDiskMap
    & Map.toList
    & selectGroups (view $ _2 % #color)
    & filter (\(color, items) -> items <&> (view $ _1 % to indexToLocation) & isFinishedSequence color)
    & map fst



-- | Sort & group items by key projection
selectGroups :: (Ord key) => (a -> key) -> [a] -> [(key, NonEmpty a)]
selectGroups projection foldable =
  foldable
  -- | decorate with the key for comparison
  & fmap (\x -> (projection x, x))
  & List.sortBy (compare `on` fst)
  & NE.groupBy ((==) `on` fst)
  -- | only list the key once per group
  & fmap (\group -> let (key, _):|_ = group in (key, fmap snd group))


-- | check if a set of disk locations is a finished sequence
isFinishedSequence :: Color -> NonEmpty Location -> Bool
isFinishedSequence color ls =
  if numberOfRings > 2
  then False
  else ls
    & fmap (onRing mostCommonRing)
    & sequenceA
    & maybe False (isFinished (diskCount color) . fmap (view #position))
  where
    diskCount :: Color -> Int
    diskCount White  = 9
    diskCount Yellow = 9
    diskCount _      = 8

    mostCommonRing :: Ring
    mostCommonRing = ringGroups & List.maximumBy (compare `on` length) & head & view #ring

    ringGroups :: [NonEmpty Location]
    ringGroups = ls & NE.groupBy ((==) `on` view #ring)

    numberOfRings :: Int
    numberOfRings = length ringGroups


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
      | x `precedes` min && (max - x < Cyclic expectedCount) = go xs x max
      -- ^ x is the new min
      | x `exceeds` max && (x - min < Cyclic expectedCount) = go xs min x
      -- ^ x is the new max
      | x `precedes` max && x `exceeds` min = go xs min max
      -- ^ x is between the min & max
      | otherwise = False
      -- ^ x is outside the band of acceptability
