module Gemini.Component.App.Actions
  ( keyboardEvents
  , scramble
  ) where

import Gemini.Prelude
import Data.Array as Array
import Data.Unfoldable (replicateA)
import Data.Gemini as Gemini
import Data.Gemini.Motions (l, l', c, c', r, r')
import Web.UIEvent.KeyboardEvent as Event
import Data.Maybe (fromJust)
import Gemini.Component.Puzzle as Puzzle
import Gemini.Store as Store
import Effect.Random (randomInt)

-- | The keyboard shortcuts are based on the top row of keys on a QWERTY keyboard
keyboardEvents :: Store Gemini -> Event.KeyboardEvent -> Effect Unit
keyboardEvents store =
  Event.key
    >>> case _ of
        "q" -> apply $ l' 1
        "w" -> apply $ l 1
        "t" -> apply $ c' 1
        "y" -> apply $ c 1
        "o" -> apply $ r' 1
        "p" -> apply $ r 1
        key -> log key
  where
  apply :: Motion -> Effect Unit
  apply = Store.modify store <<< Gemini.applyToGemini

scramble :: Store Gemini -> Effect Unit
scramble gemini = do
  randomMotions :: Array _ <- replicateA 1000 $
    ado
      m <- randomInt 1 17
      r <- randomInt 0 2
      in Motion { ring: rings `unsafeIndex` r, amount: cyclic m }

  let perm = foldMap Gemini.toPerm randomMotions
  Store.modify gemini $ Gemini.applyToGemini perm
  where
  rings = inhabitants

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex arr i = unsafePartial $ fromJust $ Array.index arr i

