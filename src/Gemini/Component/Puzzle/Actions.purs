module Gemini.Component.Puzzle.Actions
  ( drag
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


drag :: Int
drag = 42
