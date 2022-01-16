module Gemini.UI.Actions where

import           Relude

import           Data.Finitary
import           Data.Permutation
import qualified Data.Sequence          as Seq
import           Data.Traversable       (for)
import           Optics                 hiding ((#))
import           System.Random.Stateful (globalStdGen, uniformM)
import           Utils

import           Shpadoinkle
import qualified Shpadoinkle.Html       as Html
import qualified Shpadoinkle.Keyboard   as Key

import           Gemini.Types



-- | UI Operations
--
applyMotionToStore :: Motion -> Store -> Store
applyMotionToStore motion state = state
  & #gemini %~ applyToGemini motion
  & #history %~ updateHistory
  where
    updateHistory =
      if state ^. #options % #recording
      then applyToHistory motion
      else const Seq.Empty


stopRecording :: Store -> Store
stopRecording state = state
  & #options % #recording .~ False
  & #moves %~ updateMoves
  & #history .~ Seq.Empty
  where
    updateMoves =
      case state ^. #history of
        Seq.Empty -> identity
        motions   -> (toMove motions :<|)
