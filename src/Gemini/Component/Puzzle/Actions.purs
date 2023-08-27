module Gemini.Component.Puzzle.Actions
  ( onDragStart
  , onDragUpdate
  , onDragEnd
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
import Web.Event.Internal.Types as Web
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

type DragStore
  = Store (Maybe Drag)

onDragStart :: { drag :: DragStore, location :: Location } -> Web.Event -> Effect Unit
onDragStart { drag, location } event =
  drag.modify \_ ->
    Just
      { location: Gemini.dragRing location
      , chosen: Nothing
      , initialPoint: mouse
      , currentPoint: mouse
      }

  where
  mouse = unsafePointerEvent >>> point $ event

onDragUpdate :: DragStore -> Web.Event -> Effect Unit
onDragUpdate drag event = do
  maybe <- drag.ref
  case maybe of
    Nothing -> pure unit
    Just {} ->
      pure unit

onDragEnd :: { drag :: DragStore, gemini :: Store Gemini } -> Web.Event -> Effect Unit
onDragEnd { drag, gemini } event = pure unit

-- Point event utilities
type PointerEvent
  = { clientX :: Number
    , clientY :: Number
    }

-- given a mouse event / pointer event, gets the location of the event
unsafePointerEvent :: Web.Event -> PointerEvent
unsafePointerEvent = unsafeCoerce

point :: PointerEvent -> Point
point { clientX, clientY } = Point { x: clientX, y: clientY }
