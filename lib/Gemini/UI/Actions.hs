module Gemini.UI.Actions where

import           Relude

import           Data.Angle
import           Data.Finitary
import           Data.Permutation
import qualified Data.Sequence          as Seq
import           Data.Traversable       (for)
import           Optics
import           Optics.State.Operators
import           System.Random.Stateful (globalStdGen, uniformM)
import           Utils

import           Shpadoinkle
import qualified Shpadoinkle.Html       as Html
import qualified Shpadoinkle.Keyboard   as Key

import           Gemini.Types


-- | UI Operations
applyMotionToStore :: Motion -> Store -> Store
applyMotionToStore motion = execState $ do
  -- apply to puzzle
  (#gemini %= applyToGemini motion)

  -- if recording, apply to recorded move
  recording <- use $ #options % #recording
  when recording $ #recorded %= applyToHistory motion

  -- apply to history to tally move count
  (#history %= applyToHistory motion)

  -- check if the puzzle is solved
  gemini <- use #gemini
  wasScrambled <- use $ #scrambled
  when (isSolved gemini && wasScrambled) $ do
    (#options % #confetti .= True)
    (#scrambled .= False)


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
