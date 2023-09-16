module Gemini.Types
  ( Drag(..)
  , Confetti(..)
  , Stats(..)
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Gemini (Choice(..), Chosen(..), Gemini, Location, Ring, initialGemini)
import Data.Point (Point)
import Data.Timestamp (Timestamp)

type Stats =
  { scrambledAt :: Maybe Timestamp
  , solvedAt :: Maybe Timestamp
  }

type Drag =
  { location :: Choice Location
  , chosen :: Maybe Chosen
  , initialPoint :: Point
  , currentPoint :: Point
  }

data Confetti
  = Off
  | FadeIn
  | FadeOut

derive instance Eq Confetti
