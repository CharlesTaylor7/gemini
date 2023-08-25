module Gemini.Component.App.Actions
  ( keyboardEvents
  ) where

import Gemini.Prelude

import Deku.Control (text, text_)
import Deku.Listeners as Event
import Deku.Core (Nut, NutWith)
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Pursx (pursx, (~~), (~!~))
import Deku.Attributes (klass_, href_)
import Deku.Attribute (xdata, (!:=))
import Deku.Attribute as Attr
import Deku.Hooks (useState)
import Deku.Hooks.UseStore (Store, modify)
import Deku.Listeners as Listener
import Deku.Extra (className)

import Data.Gemini as Gemini 
import Web.UIEvent.KeyboardEvent as Event

import Gemini.Env (Env)
import Gemini.Component.Puzzle as Puzzle


type KeyboardEvent = { key :: String }

type Pusher a = a -> Effect Unit
type Hook a = Effect a /\ Pusher a

-- | The keyboard shortcuts are based on the top row of keys on a QWERTY keyboard
keyboardEvents :: forall e. Store AppState -> Event (Attribute e)
keyboardEvents store =
  Listener.keyDown_ $ Event.key >>> case _ of
    "Q" -> apply $ rotation LeftRing AntiClockwise
    "W" -> apply $ rotation LeftRing Clockwise
    "T" -> apply $ rotation CenterRing AntiClockwise
    "Y" -> apply $ rotation CenterRing Clockwise
    "O" -> apply $ rotation RightRing AntiClockwise
    "P" -> apply $ rotation RightRing Clockwise
    _   -> pure unit
    where
      apply :: Rotation -> Effect Unit
      apply = modify store <<< overGemini <<< applyRotation

overGemini :: (Gemini -> Gemini) -> AppState -> AppState
overGemini f s = s { gemini = f s.gemini }

applyRotation :: Rotation -> Gemini -> Gemini
applyRotation r = Gemini.applyToGemini $ toMotion r

toMotion :: Rotation -> Motion
toMotion rotation = Motion { amount: cyclic 1, rotation }


