module Gemini.Solve
  ( solutionPairs
  , nextMove
  , toSolveStage
  , Orientation(..)
  , BotMove(..)
  ) where

import           Optics
import           Relude

import           Data.Cyclic        as Cyclic
import           Data.Gemini        as Gemini
import           Gemini.Solve.Types
import           Gemini.Types       ()


newtype BotMove = BotMove [Motion]


closestDisk :: Gemini -> Color -> Ring -> DiskIndex -> Maybe DiskIndex
closestDisk g color ring pivot =
  closest g pivot $
  ifiltered (\_ disk -> disk ^. #color == color) $
  Gemini.disksOf ring


closest :: Is k A_Fold => Gemini -> DiskIndex -> Optic' k '[DiskIndex] Gemini a -> Maybe DiskIndex
closest g pivot optic = ifoldlOf' optic closer Nothing g
  where
    closer i Nothing _  = Just i
    closer i (Just j) _ = Just $
      if Cyclic.distance pivot i < Cyclic.distance pivot j then i else j


nextMove :: Gemini -> BotMove
nextMove g =
  case toSolveStage g of
    StageRed ->
        case closestDisk g Red CenterRing 11 of
          Just n -> BotMove [c (11 - n), l 1]
          _           ->
            case closestDisk g Red RightRing 11 of
              Just n -> BotMove [ r (11 - n), c 4, l 1]
              _      -> noMove
    _ -> noMove

  where
    noMove = BotMove []


type Pair a = (a, a)

-- TODO: Use `compareCyclic` == Precedes / Exceeds + color info to determine when pairs are inverted
data Orientation = Oriented | Inverted

pair :: Location -> Location -> Pair Location
pair (canonical -> x) (canonical -> y) = if x <= y then (x, y) else (y, x)

toSolveStage :: Gemini -> BotSolveState
toSolveStage g = do
  let redLocations = fromList $ Location LeftRing . Cyclic <$> [7..15] :: Set Location
  let firstRed =
        iheadOf $
        ifiltered (\loc disk -> disk ^. #color == Red && redLocations ^. contains loc % to not) $
        each

  case firstRed g of
    Just _ -> StageRed
    _      -> StageYellow


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
