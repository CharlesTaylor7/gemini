module Gemini.Solve.Types where

import           Relude

import           Data.Aeson (FromJSON, ToJSON)

data BotSolveState = StageRed | StageYellow | StageBlack | StageBlue | StageGreen | Finale
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)
