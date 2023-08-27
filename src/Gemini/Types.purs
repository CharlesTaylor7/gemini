module Gemini.Types
  ( AppState(..)
  , initialAppState
  , HoverState(..)
  , DragState(..)
  , Options
  , Env(..)
  , Deployment(..)
  , Confetti(..)
  , Stats(..)
  , Move(..)
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Gemini (Choice, Chosen, Gemini, Location, Ring, initialGemini)
import Data.Permutation (Cycles, Cycle)
import Data.Point (Point)
import Data.Timestamp (Timestamp)

type Stats
  = { scrambledAt :: Timestamp
    , solvedAt :: (Maybe Timestamp)
    }

type HoverState
  = { move :: Move
    , cycle :: Maybe (Cycle Location)
    }

type Move
  = {}
{- 
type Move = 
  { motions    :: (Seq Motion)
  , moveCycles :: (Cycles Location)
  }
-}
type DragState
  = { location :: Choice Location
    , chosen :: Maybe Chosen
    , initialPoint :: Point
    , currentPoint :: Point
    }

type Options
  = { showLabels :: Boolean
    , showKeyboardShortcuts :: Boolean
    , recording :: Boolean
    , confetti :: Confetti
    , highlightPairs :: Boolean
    , mobile :: Boolean
    }

data Confetti
  = Off
  | FadeIn
  | FadeOut
derive instance Eq Confetti

type Env
  = { port :: Int
    , commit :: String
    , deployment :: Deployment
    }

data Deployment
  = Prod
  | Dev

type AppState
  = { gemini :: Gemini
    -- , history   :: (Seq Motion)
    -- , buffered  :: (Seq Motion)
    -- , animation :: Animation
    , hover :: (Maybe HoverState)
    , drag :: (Maybe DragState)
    , options :: Options
    -- , env       :: Env
    -- , moves     :: (Seq Move)
    -- , recorded  :: (Seq Motion)
    -- , stats     :: (Maybe Stats)
    -- , errors    :: Array String
    }

-- | Initial state of the app
initialAppState :: AppState
initialAppState =
  { gemini: initialGemini
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
  --, stats: Nothing
  --, errors: []
  }
