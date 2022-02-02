module Gemini.Solve
  ( solutionPairs
  ) where

import           Relude

import           Data.Gemini
import           Optics


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
