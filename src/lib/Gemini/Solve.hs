{-# options_ghc -Wwarn #-}
module Gemini.Solve where

import           Relude

import           Data.Finitary
import           Data.Gemini
import           Data.List     ((\\))
import           Optics


type Pair a = (a, a)


pair :: a -> a -> Pair a
pair x y = (x, y)


highlightPairs :: Gemini -> [Pair Location]
highlightPairs gemini =
  gemini
  & itoListOf each
  & filter (\(_, Disk { color }) -> color `notElem` solved)
  & concatMap toPairs
  where
    solved = solvedColors gemini

    toPairs :: (Location, Disk) -> [Pair Location]
    toPairs (location, disk) = pair location <$> do
      locations <- location : (sibling location & toList)
      let candidate = location & #position %~ (+ 5)
      let check color = color `notElem` (disk ^. #color : solved)
      guard $ maybe False check $ gemini ^? geminiIx candidate % #color
      pure $ candidate
