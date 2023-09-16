module Deku.Extra
  ( autoFocus
  , tabIndex
  , keyboard
  , mouse
  , touch
  , Pusher
  , module FRP.Event
  , module Web.UIEvent.KeyboardEvent
  , MouseEvent
  ) where

import Debug
import Prelude

import Control.Apply (lift2)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute (class Attr, Attribute, AttributeValue(..), Cb, cb, unsafeAttribute)
import Deku.Attributes (klass)
import Deku.DOM as H
import Effect (Effect)
import FRP.Event (Event)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

type Pusher a = a -> Effect Unit

keyboard :: (KeyboardEvent -> Effect Unit) -> Cb
keyboard f = cb $ f <<< convert
  where
  convert :: Web.Event -> KeyboardEvent
  convert = unsafeCoerce

mouse :: (MouseEvent -> Effect Unit) -> Cb
mouse f =
  cb $ f <<< convert
  where
  convert :: Web.Event -> MouseEvent
  convert = unsafeCoerce

touch :: (Touch -> Effect Unit) -> Cb
touch f =
  cb $ f <<< (\r -> r.changedTouches.item 0) <<< coerceEvent
  where
  coerceEvent :: Web.Event -> TouchEvent
  coerceEvent = unsafeCoerce

type MouseEvent =
  { clientX :: Number
  , clientY :: Number
  }

type TouchEvent =
  { changedTouches :: { item :: Int -> Touch }
  }

type Touch =
  { clientX :: Number
  , clientY :: Number
  }

autoFocus :: forall e. Event (Attribute e)
autoFocus = pure $ unsafeAttribute { key: "autofocus", value: Prop' "" }

tabIndex :: forall e. Event Int -> Event (Attribute e)
tabIndex = map
  (\index -> unsafeAttribute { key: "tabIndex", value: Prop' $ show index })
