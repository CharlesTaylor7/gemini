module Gemini.Types
  ( -- ui types
    Store(..), initialStore
  , HoverState(..), DragState(..), Options(..), Env(..), Deployment(..), DomInfo(..) , Confetti(..)
  , Stats(..)
  , Motion(..), Move(..), normalize
  , Timestamp(..)
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
  , module Timestamp
  ) where

import           Optics
import           Relude             hiding (cycle)

import           Data.Angle         as Angle
import           Data.Cyclic        as Cyclic
import           Data.Finitary      as Finitary
import           Data.Gemini        as Gemini
import           Data.Permutation   as Permutation
import           Data.Point         as Point
import           Data.Timestamp     as Timestamp

import qualified Data.Sequence      as Seq
import qualified Prettyprinter      as Pretty

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Sequence      (Seq ((:<|), (:|>)))
import           Prettyprinter      (Pretty (..), (<+>))

import           Gemini.Solve.Types (BotSolveState (..), initialSolveState)


-- | UI Definitions
data Store = Store
  { gemini        :: !Gemini
  , history       :: !(Seq Motion)
  , hover         :: !(Maybe HoverState)
  , drag          :: !(Maybe DragState)
  , options       :: !Options
  , env           :: !Env
  , dom           :: !DomInfo
  , moves         :: !(Seq Move)
  , recorded      :: !(Seq Motion)
  , stats         :: !(Maybe Stats)
  , botSolveState :: !BotSolveState
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)


data Stats = Stats
  { scrambledAt :: !Timestamp
  , solvedAt    :: !(Maybe Timestamp)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)



data HoverState = HoverState
  { move  :: !Move
  , cycle :: !(Maybe (Cycle Location))
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

instance Pretty HoverState where
  pretty HoverState { move, cycle } = pretty move <+> pretty cycle


data DragState = DragState
  { location     :: !(Choice Location)
  , chosen       :: !(Maybe Chosen)
  , initialPoint :: !Point
  , currentPoint :: !Point
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

instance Pretty DragState where
  pretty DragState { location, chosen, initialPoint, currentPoint }
    = Pretty.sep [pretty location, Pretty.viaShow chosen, pretty initialPoint, pretty currentPoint ]


data DomInfo = DomInfo
  { ringCenters :: !(Map Ring Point)
  , ringRadius  :: !Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)


data Options = Options
  { showLabels            :: !Bool
  , showKeyboardShortcuts :: !Bool
  , showBotControls       :: !Bool
  , recording             :: !Bool
  , debug                 :: !Bool
  , confetti              :: !Confetti
  , highlightPairs        :: !Bool
  , mobile                :: !Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)


data Confetti = Off | FadeIn | FadeOut
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)


data Env = Env
  { port       :: !Int
  , commit     :: !Text
  , deployment :: !Deployment
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)


data Deployment
  = Prod
  | Dev
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)


-- | Core Definitions
data Move = Move
  { motions    :: !(Seq Motion)
  , moveCycles :: !(Cycles Location)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

instance Pretty Move where
  pretty = prettyList . toList . view #motions


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


-- | Initial state of the app
initialStore :: Env -> Store
initialStore env = Store
  { gemini = initialGemini
  , history = Seq.Empty
  , recorded = Seq.Empty
  , moves = Seq.Empty
  , hover = Nothing
  , drag = Nothing
  , options = Options
      { showLabels = False
      , recording = False
      , debug = False
      , mobile = False
      , confetti = Off
      , highlightPairs = False
      , showKeyboardShortcuts = False
      , showBotControls = True
      }
  , env = env
  , dom = DomInfo
    { ringCenters = mempty
    , ringRadius = 0
    }
  , stats = Nothing
  , botSolveState = initialSolveState
  }

