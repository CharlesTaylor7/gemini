module Gemini.Types
  ( -- ui types
    Store(..), initialStore
  , Action
  , HoverState(..), DragState(..), Options(..), Env(..), Deployment(..), DomInfo(..) , Confetti(..)
  , Animation(..)
  , AnimationFrame(..)
  , Stats(..)
  , Motion(..), Move(..), normalize
  , Timestamp(..)
    -- re export Seq constructors
  , pattern (:<|), pattern (:|>)
  -- re export other modules
  , Group(..)
  , Rotation(..)
  , GeminiPermutation
  , Location(..)
  , module Angle
  , module Cyclic
  , module Finitary
  , module Permutation
  , module Point
  , module Timestamp
  ) where

import           Optics
import           Relude           hiding (cycle)

import           Data.Angle       as Angle
import           Data.Cyclic      as Cyclic
import           Data.Finitary    as Finitary
import           Data.Gemini      as Gemini
import           Data.Permutation as Permutation
import           Data.Point       as Point
import           Data.Timestamp   as Timestamp

import qualified Data.Sequence    as Seq
import qualified Prettyprinter    as Pretty

import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Sequence    (Seq ((:<|), (:|>)))
import           Prettyprinter    (Pretty (..), (<+>))

import           Gemini.FFI       (IntervalId)


-- | UI Definitions
type Action m = ExceptT Text (StateT Store m)

data Animation = Animation
  { frame            :: !(Maybe AnimationFrame)
  , ticksPerRotation :: !Int
  , refreshRate      :: !Int
  , intervalId       :: !(Maybe IntervalId)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

instance Pretty Animation where
  pretty Animation { frame, ticksPerRotation } =
    Pretty.vsep
      [ pretty ticksPerRotation
      , pretty frame
      ]

data AnimationFrame = AnimationFrame
  { tick   :: !Int
  , motion :: !Motion
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

instance Pretty AnimationFrame where
  pretty AnimationFrame { tick, motion } =
    pretty tick <+> pretty motion

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
  pretty HoverState { move, cycle } =
    pretty move <+> pretty cycle


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
  , recording             :: !Bool
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


data Store = Store
  { gemini    :: !Gemini
  , history   :: !(Seq Motion)
  , buffered  :: !(Seq Motion)
  , animation :: !Animation
  , hover     :: !(Maybe HoverState)
  , drag      :: !(Maybe DragState)
  , options   :: !Options
  , env       :: !Env
  , dom       :: !DomInfo
  , moves     :: !(Seq Move)
  , recorded  :: !(Seq Motion)
  , stats     :: !(Maybe Stats)
  , errors    :: ![Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)



-- | Initial state of the app
initialStore :: Env -> Store
initialStore env = Store
  { gemini = initialGemini
  , history = Seq.Empty
  , buffered = Seq.Empty
  , animation = Animation
    { frame = Nothing
    , ticksPerRotation = 10
    , refreshRate = 30
    , intervalId = Nothing
    }
  , recorded = Seq.Empty
  , moves = Seq.Empty
  , hover = Nothing
  , drag = Nothing
  , options = Options
      { showLabels = False
      , recording = False
      , mobile = False
      , confetti = Off
      , highlightPairs = False
      , showKeyboardShortcuts = False
      }
  , env = env
  , dom = DomInfo
    { ringCenters = mempty
    , ringRadius = 0
    }
  , stats = Nothing
  , errors = []
  }
