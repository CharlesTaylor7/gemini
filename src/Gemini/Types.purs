module Gemini.Types
  ( Hover(..)
  , Drag(..)
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
  = { scrambledAt :: Maybe Timestamp
    , solvedAt :: Maybe Timestamp
    }

type Hover
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
type Drag
  = { location :: Choice Location
    , chosen :: Maybe Chosen
    , initialPoint :: Point
    , currentPoint :: Point
    }

data Confetti
  = Off
  | FadeIn
  | FadeOut
derive instance Eq Confetti
