module Gemini.Solve.Types where

import           Relude

import           Data.Aeson (FromJSON, ToJSON)

initialSolveState :: BotSolveState
initialSolveState =
  BotSolveState
  { stage = StageRed $ StageRedState { redCount = 0 }
  }

data BotSolveState = BotSolveState
  { stage  :: !Stage
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

data Stage = StageRed StageRedState | StageYellow | StageBlack | StageBlue | StageGreen | Finale
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)

data StageRedState = StageRedState
  { redCount :: !Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (NFData)
