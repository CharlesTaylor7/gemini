module Gemini.Solve
  ( solutionPairs
  , nextMove
  , Orientation(..)
  , BotMove(..)
  ) where

import           Optics
import           Relude

import           Data.Gemini        as Gemini
import           Gemini.Solve.Types
import           Gemini.Types


data BotMove = BotMove [Motion] BotSolveState

nextMove :: Gemini -> BotSolveState -> BotMove
nextMove g =
  \case
    solve@BotSolveState { stage = StageRed StageRedState { redCount } } ->
      let
        Just (n, _) =
          g & (
            iheadOf $
            ifiltered (\loc disk -> disk ^. #color == Red) $
            Gemini.disksOf CenterRing
          )



      in
        BotMove [c (11 - n), l 1] solve



type Pair a = (a, a)

-- TODO: Use `compareCyclic` == Precedes / Exceeds + color info to determine when pairs are inverted
data Orientation = Oriented | Inverted

pair :: Location -> Location -> Pair Location
pair (canonical -> x) (canonical -> y) = if x <= y then (x, y) else (y, x)


solutionPairs :: Gemini -> Set (Pair Location)
solutionPairs gemini =
  gemini
  & itoListOf each
  & filter (\(_, Disk { color }) -> color `notElem` solved)
  & concatMap toPairs
  & fromList
  where
    solved = solvedColors gemini

    toPairs :: (Location, Disk) -> [Pair Location]
    toPairs (location, disk) = pair location <$> do
      offset <- [-5, 5]
      location <- location : (sibling location & toList)
      let candidate = location & #position %~ (+ offset)
      let check color = color `notElem` (disk ^. #color : solved)
      guard $ maybe False check $ gemini ^? geminiIx candidate % #color
      pure $ candidate


