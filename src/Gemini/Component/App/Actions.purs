module Gemini.Component.App.Actions
  ( keyboardEvents
  ) where

import Gemini.Prelude

import Deku.Control (text, text_)
import Deku.Core (Nut, NutWith)
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Pursx (pursx, (~~), (~!~))
import Deku.Attributes (klass_, href_)
import Deku.Attribute (xdata, (!:=))
import Deku.Attribute as Attr
import Deku.Hooks (useState)
import Deku.Hooks.UseStore (Store)
import Deku.Listeners as Listener
import Deku.Extra (className)

import Data.Gemini as Gemini 
import Web.UIEvent.KeyboardEvent as Event

import Gemini.Env (Env)
import Gemini.Component.Puzzle as Puzzle
import Effect.Random (randomInt)


type KeyboardEvent = { key :: String }

type Pusher a = a -> Effect Unit
type Hook a = Effect a /\ Pusher a

-- | The keyboard shortcuts are based on the top row of keys on a QWERTY keyboard
keyboardEvents :: Store AppState -> Event.KeyboardEvent -> Effect Unit
keyboardEvents store =
  Event.key >>> case _ of
    "q" -> apply $ rotation LeftRing AntiClockwise
    "w" -> apply $ rotation LeftRing Clockwise
    "t" -> apply $ rotation CenterRing AntiClockwise
    "y" -> apply $ rotation CenterRing Clockwise
    "o" -> apply $ rotation RightRing AntiClockwise
    "p" -> apply $ rotation RightRing Clockwise
    key   -> log key
    where
      apply :: Rotation -> Effect Unit
      apply = store.modify <<< overGemini <<< applyRotation

      applyRotation :: Rotation -> Gemini -> Gemini
      applyRotation r = Gemini.applyToGemini $ toMotion r

      toMotion :: Rotation -> Motion
      toMotion rotation = Motion { amount: cyclic 1, rotation }

scramble :: forall e. Store AppState -> Effect Unit
scramble store = do


overGemini :: (Gemini -> Gemini) -> AppState -> AppState
overGemini f s = s { gemini = f s.gemini }
