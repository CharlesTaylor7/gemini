module Gemini.Component.App.Actions
  ( keyboardEvents
  ) where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)

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
import Deku.Listeners as Listener
import Deku.Extra (className)

import Data.Gemini as Gemini 

import Gemini.Env (Env)
import Gemini.Component.Puzzle as Puzzle
import Gemini.Types (initialStore, Store)

keyboardEvents :: Int
keyboardEvents = 2

type KeyboardEvent = { key :: String }

type Pusher a = a -> Effect Unit
type Hook a = Effect a /\ Pusher a
{-
keyboardMotions :: forall e. Effect Gemini -> Event (Attribute e)
keyboardMotions =
  Listener.onKeydown_ $ \case
    -- the keyboard shortcuts are based on the top row of keys on a QWERTY keyboard
    Key.Q -> apply $ Rotation LeftRing AntiClockwise
    Key.W -> apply $ Rotation LeftRing Clockwise
    Key.T -> apply $ Rotation CenterRing AntiClockwise
    Key.Y -> apply $ Rotation CenterRing Clockwise
    Key.O -> apply $ Rotation RightRing AntiClockwise
    Key.P -> apply $ Rotation RightRing Clockwise
    _     -> Continuation.pur identity
    where
      apply = Actions.toContinuation . applyRotation


-}
