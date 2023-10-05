module Gemini.Component.App.Actions
  ( keyboardEvents
  , scramble2
  ) where

import Data.Gemini.Gen
import Gemini.Prelude

import Data.Array as Array
import Data.Gemini as Gemini
import Data.Gemini.Motions (c, c', l, l', r, r')
import Data.Gemini.Solve as Gemini
import Data.Maybe (fromJust)
import Data.Unfoldable (replicateA)
import Effect.Console as Console
import Effect.Random (randomInt)
import Gemini.Store as Store
import Random.LCG (mkSeed)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Web.UIEvent.KeyboardEvent as Event

-- | The keyboard shortcuts are based on the top row of keys on a QWERTY keyboard
keyboardEvents ::
  forall rest.
  { gemini :: Store Gemini
  , pushConfetti :: Pusher Confetti
  | rest
  } ->
  Event.KeyboardEvent ->
  Effect Unit
keyboardEvents { gemini, pushConfetti } =
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
  apply motion = do
    Store.modify gemini $ Gemini.applyToGemini motion
    gemini <- Store.read gemini
    when (Gemini.isSolved gemini)
      $ pushConfetti FadeIn

scramble2 :: Store Gemini -> Effect Unit
scramble2 store =
  Store.set store $
    Gen.evalGen
      (arbitrary <#> \(AlmostSolvedGemini g) -> g)
      { newSeed: mkSeed 1847507541, size: 0 }

scramble :: Store Gemini -> Effect Unit
scramble store = do
  randomMotions :: Array _ <-
    replicateA 1000
      $ ado
          m <- randomInt 1 17
          r <- randomInt 0 2
          in Motion { ring: rings `unsafeIndex` r, amount: cyclic m }
  let perm = foldMap Gemini.toPerm randomMotions
  Store.modify store $ Gemini.permuteGemini perm
  where
  rings = inhabitants

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex arr i = unsafePartial $ fromJust $ Array.index arr i
