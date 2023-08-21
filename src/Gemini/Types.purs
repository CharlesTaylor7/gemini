module Gemini.Types
  ( -- ui types
    Store(..), initialStore
  --, Action
  , HoverState(..), DragState(..), Options(..), Env(..), Deployment(..), DomInfo(..) , Confetti(..)
  , Stats(..)
  , Move(..)
  ) where

import           Prelude
-- import           Data.Angle       as Angle
-- import           Data.Cyclic      as Cyclic
import           Data.Finitary    as Finitary
import           Data.Gemini      as Gemini
-- import           Data.Permutation as Permutation
-- import           Data.Point       as Point
import           Data.Timestamp   as Timestamp



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
  , ringRadius  :: Double
  }


type Options = 
  { showLabels            :: Bool
  , showKeyboardShortcuts :: Bool
  , recording             :: Bool
  , confetti              :: Confetti
  , highlightPairs        :: Bool
  , mobile                :: Bool
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
  { motions    :: (Seq Motion)
  , moveCycles :: (Cycles Location)
  }


type Store = Store
  { gemini    :: Gemini
  , history   :: (Seq Motion)
  , buffered  :: (Seq Motion)
  , animation :: Animation
  , hover     :: (Maybe HoverState)
  , drag      :: (Maybe DragState)
  , options   :: Options
  , env       :: Env
  , dom       :: DomInfo
  , moves     :: (Seq Move)
  , recorded  :: (Seq Motion)
  , stats     :: (Maybe Stats)
  , errors    :: Array String
  }



-- | Initial state of the app
initialStore :: Env -> Store
initialStore env = 
  { gemini: Gemini.initialGemini
  , history: Seq.Empty
  , buffered: Seq.Empty
  , recorded: Seq.Empty
  , moves: Seq.Empty
  , hover: Nothing
  , drag: Nothing
  , options: Options
      { showLabels: False
      , recording: False
      , mobile: False
      , confetti: Off
      , highlightPairs: False
      , showKeyboardShortcuts: False
      }
  , env: env
  , dom: DomInfo
    { ringCenters: mempty
    , ringRadius: 0
    }
  , stats: Nothing
  , errors: []
  }
