module Gemini.UI.Actions where

import           Relude

import           Data.Angle
import           Data.Finitary
import           Data.Permutation
import qualified Data.Sequence          as Seq
import           Optics
import           Optics.State.Operators

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

    toMove :: Seq Motion -> Move
    toMove motions = Move { motions, moveCycles = locationCycles $ toPerm motions }

    locationCycles :: GeminiPermutation -> Cycles Location
    locationCycles = fmap indexToLocation . toCycles


applyRotation :: Rotation -> Store -> Store
applyRotation r = applyMotionToStore $ toMotion r
  where
    toMotion :: Rotation -> Motion
    toMotion rotation = Motion { amount = 1, rotation }


-- | Apply a new motion to an existing history of motions
-- Collapses motions of the same ring into 1 larger motion.
-- Allows undoing motions.
-- Removes trivial motions that do nothing.
applyToHistory :: Motion -> Seq Motion -> Seq Motion
applyToHistory motion Seq.Empty           = fromList [ motion ]
applyToHistory next all@(ms :|> prev) =
  if next ^. #rotation % #ring /= prev ^. #rotation % #ring
  then all :|> next
  else case normalize (combine prev next) of
    Just m -> ms :|> m
    _      -> ms
  where
    combine :: Motion -> Motion -> Motion
    combine x@(Motion m1 r1) (Motion m2 r2)
      | r1 == r2  = x & #amount %~ (+ m2)
      | otherwise = x & #amount %~ (subtract m2)
