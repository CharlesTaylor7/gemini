module Gemini.Types
  ( -- ui types
    Store(..), HoverState(..), DragState(..), Options(..), Env(..), Deployment(..), DomInfo(..)
  , Motion(..), Move(..), normalize
    -- re export Seq constructors
  , pattern (:<|), pattern (:|>)
  -- re export other modules
  , Group(..)
  , module Angle
  , module Cyclic
  , module Finitary
  , module Gemini
  , module Permutation
  , module Point
  ) where

import           Relude           hiding (cycle)

import           Data.Angle       as Angle
import           Data.Cyclic      as Cyclic
import           Data.Finitary    as Finitary
import           Data.Gemini      as Gemini
import           Data.Permutation as Permutation
import           Data.Point       as Point

import           Data.Sequence    (Seq ((:<|), (:|>)))
import qualified Data.Sequence    as Seq
import           Optics
import           Prettyprinter    (Pretty (..))
import qualified Prettyprinter    as Pretty


-- | UI Definitions
data Store = Store
  { gemini    :: !Gemini
  , history   :: !(Seq Motion)
  , scrambled :: !Bool
  , hover     :: !HoverState
  , drag      :: !(Maybe DragState)
  , options   :: !Options
  , env       :: !Env
  , dom       :: !DomInfo
  -- | record moves
  , moves     :: !(Seq Move)
  , recorded  :: !(Seq Motion)
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
  , chosen       :: !(Maybe Chosen)
  , initialPoint :: !Point
  , currentPoint :: !Point
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Pretty DragState where
  pretty DragState { location, chosen, initialPoint, currentPoint }
    = Pretty.sep [pretty location, Pretty.viaShow chosen, pretty initialPoint, pretty currentPoint ]


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
