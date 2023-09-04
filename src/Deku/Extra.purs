module Deku.Extra
  ( autoFocus
  , tabIndex
  , keyboard
  , pointer
  , touch
  , Pusher
  , module FRP.Event
  , module Web.UIEvent.KeyboardEvent
  , PointerEvent
  ) where

import Prelude
import Effect (Effect)
import FRP.Event (Event)
import Data.Foldable (foldMap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Control.Apply (lift2)
import Deku.DOM as H
import Deku.Attribute (Attribute, class Attr, cb, Cb, unsafeAttribute, AttributeValue(..))
import Deku.Attributes (klass)
import Web.Event.Event as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Unsafe.Coerce (unsafeCoerce)
import Safe.Coerce (coerce)
import Debug

type Pusher a
  = a -> Effect Unit

keyboard :: (KeyboardEvent -> Effect Unit) -> Cb
keyboard f = cb $ f <<< convert
  where
  convert :: Web.Event -> KeyboardEvent
  convert = unsafeCoerce

pointer :: String -> (PointerEvent -> Effect Unit) -> Cb
pointer f =
  cb $ f <<< convert
  where
  convert :: Web.Event -> PointerEvent
  convert = unsafeCoerce

touch :: String -> (Touch -> Effect Unit) -> Cb
touch f =
  cb $ f <<< (\r -> r.changedTouches.item 0) <<< coerceEvent
  where
  coerceEvent :: Web.Event -> TouchEvent
  coerceEvent = unsafeCoerce

type PointerEvent
  = { clientX :: Number
    , clientY :: Number
    }

type TouchEvent
  = { changedTouches :: { item :: Int -> Touch }
    }

type Touch
  = { clientX :: Number
    , clientY :: Number
    }

autoFocus :: forall e. Event (Attribute e)
autoFocus = pure $ unsafeAttribute { key: "autofocus", value: Prop' "" }

tabIndex :: forall e. Event Int -> Event (Attribute e)
tabIndex = map (\index -> unsafeAttribute { key: "tabIndex", value: Prop' $ show index })
