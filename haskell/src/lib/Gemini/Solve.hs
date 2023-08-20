{-# options_GHC -Wwarn #-}
module Gemini.Solve
  ( solutionPairs
  , nextMove
  , toSolveStage
  , Orientation(..)
  , BotMove(..)
  ) where


import           Optics
import           Optics.Core.Extras (is)
import           Relude

import           Data.Cyclic        as Cyclic
import           Data.Gemini        as Gemini
import           Gemini.Solve.Types


newtype BotMove = BotMove [Motion]
botMove :: [Motion] -> BotMove
botMove = BotMove . toListOf (each % to normalize % #_Just)


disks :: Color -> Ring -> IxFold DiskIndex Gemini Disk
disks color ring =
  ifiltered (\_ disk -> disk ^. #color == color) $
  Gemini.disksOf ring

excluding :: Ord i => IxFold i g a -> Set i -> IxFold i g a
excluding f s = ifiltered (\i _ -> s ^. contains i % to not) f


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
        case closest g 11 (disks Red CenterRing) of
          Just n -> botMove [c (11 - n), l 1]
          _           ->
            case closest g 11 (disks Red RightRing) of
              Just n -> botMove [ r (11 - n), c 4, l 1]
              _      ->
                case (firstRed, firstNotInSequence) of
                  (Just m, Just n) ->
                    case m `compareCyclic` 2 of
                      -- exceeds
                      Exceeds ->
                        botMove [l (7 - m), c 1, l (7 - m), c (-1), l 1]

                      -- precedes
                      _       ->
                        botMove [l (2 - m), c (-1), l (5 - m + n), c 4, l (n - 2)]
                  _ -> noMove

    _ -> noMove

  where
    noMove = BotMove []
    firstRed :: Maybe DiskIndex
    firstRed =
      g & (
        headOf $
          Gemini.disksFrom LeftRing [6,5..] %
          filtered (is $ _2 % #color % #_Red) %
          _1
      )

    firstNotInSequence :: Maybe DiskIndex
    firstNotInSequence =
      g & (
        headOf $
          Gemini.disksFrom LeftRing [8..] %
          filtered (isn't $ _2 % #color % #_Red) %
          _1

      )



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
