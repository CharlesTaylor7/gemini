module Gemini.Component.App.Actions
  ( keyboardEvents
  , scramble
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

import Data.Array as Array
import Data.Unfoldable
import Data.Gemini as Gemini 
import Data.Gemini.Motions (l, l', c, c', r, r') 
import Web.UIEvent.KeyboardEvent as Event
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

import Gemini.Env (Env)
import Gemini.Component.Puzzle as Puzzle
import Effect.Random (randomInt)

type KeyboardEvent = { key :: String }

type Pusher a = a -> Effect Unit
type Hook a = Effect a /\ Pusher a

-- | The keyboard shortcuts are based on the top row of keys on a QWERTY keyboard
keyboardEvents :: Store Gemini -> Event.KeyboardEvent -> Effect Unit
keyboardEvents store =
  Event.key >>> case _ of
    "q" -> apply $ l' 1
    "w" -> apply $ l 1
    "t" -> apply $ c' 1
    "y" -> apply $ c 1
    "o" -> apply $ r' 1
    "p" -> apply $ r 1
    key -> log key
    where
      apply :: Motion -> Effect Unit
      apply = store.modify <<< Gemini.applyToGemini


scramble :: Store Gemini -> Effect Unit
scramble store = do
  randomInts :: Array _ <- replicateA 1000 $ randomInt 0 5
  let perm = foldMap (\i -> Gemini.toPerm (actions `unsafeIndex` i)) randomInts
  store.modify $ Gemini.applyToGemini perm
  where
    actions = [ l 1, l' 1, c 1, c' 1, r 1, r' 1]

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex arr = Array.index arr >>> unsafeFromJust

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust m = unsafePartial $ fromJust m
