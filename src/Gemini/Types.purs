module Gemini.Types
  ( -- ui types
    Store(..), initialStore
  --, Action
  , HoverState(..), DragState(..), Options, Env(..), Deployment(..), DomInfo(..) , Confetti(..)
  , Stats(..)
  , Move(..)
  -- Reexports
  , module Gemini
  ) where

import Prelude
import Data.Maybe(Maybe(..))
import Data.Angle       as Angle
import Data.Cyclic      as Cyclic
import Data.Finitary    as Finitary
import Data.Map (Map)
import Data.Map as Map
import Data.Nat
import Data.Gemini      ( Gemini, Location(..), Ring(..), Choice, Chosen )
import Data.Gemini      as Gemini
import Data.Permutation (Cycles, Cycle)
import Data.Permutation as Permutation
import Data.Point (Point)
import Data.Timestamp (Timestamp)



-- | UI Definitions

type Stats = 
  { scrambledAt :: Timestamp
  , solvedAt    :: (Maybe Timestamp)
  }


type HoverState = 
  { move  :: Move
  , cycle :: (Maybe (Cycle Location))
  }


type DragState = 
  { location     :: (Choice Location)
  , chosen       :: (Maybe Chosen)
  , initialPoint :: Point
  , currentPoint :: Point
  }


type DomInfo = 
  { ringCenters :: (Map Ring Point)
  , ringRadius  :: Number
  }


type Options = 
  { showLabels  :: Boolean
  , showKeyboardShortcuts :: Boolean
  , recording   :: Boolean
  , confetti    :: Confetti
  , highlightPairs :: Boolean
  , mobile      :: Boolean
  }


data Confetti = Off | FadeIn | FadeOut
derive instance Eq Confetti

type Env = 
  { port       :: Int
  , commit     :: String
  , deployment :: Deployment
  }


data Deployment
  = Prod
  | Dev


-- | Core Definitions
type Move = 
  --{ motions    :: (Seq Motion)
  { moveCycles :: (Cycles Location)
  }


type Store = 
  { gemini    :: Gemini
  -- , history   :: (Seq Motion)
  -- , buffered  :: (Seq Motion)
  -- , animation :: Animation
  , hover     :: (Maybe HoverState)
  , drag      :: (Maybe DragState)
  , options   :: Options
  -- , env       :: Env
  , dom       :: DomInfo
  -- , moves     :: (Seq Move)
  -- , recorded  :: (Seq Motion)
  -- , stats     :: (Maybe Stats)
  -- , errors    :: Array String
  }



-- | Initial state of the app
initialStore :: Store
initialStore = 
  { gemini: Gemini.initialGemini
  -- | Every isntance of Seq, can probably be replaced with CatQueue 
  -- | a double ended catenable queue
  -- | CatList is a one way queue only
  -- , history: Seq.Empty
  -- , buffered: Seq.Empty
  -- , recorded: Seq.Empty
  -- , moves: Seq.Empty
  , hover: Nothing
  , drag: Nothing
  , options: 
      { showLabels: false
      , recording: false
      , mobile: false
      , confetti: Off
      , highlightPairs: false
      , showKeyboardShortcuts: false
      }
  -- , env: env
  , dom: 
    { ringCenters: Map.empty
    , ringRadius: 0.0
    }
  --, stats: Nothing
  --, errors: []
  }
