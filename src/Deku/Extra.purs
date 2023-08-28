module Deku.Extra
  ( className
  , autoFocus
  , tabIndex
  , keyboard
  , pointer
  , Pusher
  , module FRP.Event
  , module Web.UIEvent.KeyboardEvent
  , PointerEvent
  ) where

import Prelude
import Effect (Effect)
import FRP.Event (Event)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Control.Apply (lift2)
import Deku.DOM as H
import Deku.Attribute (Attribute, class Attr, cb, Cb, unsafeAttribute, AttributeValue(..))
import Deku.Attributes (klass)
import Web.Event.Event as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Unsafe.Coerce (unsafeCoerce)

type Pusher a
  = a -> Effect Unit

keyboard :: (KeyboardEvent -> Effect Unit) -> Cb
keyboard f = cb $ f <<< convert
  where
  convert :: Web.Event -> KeyboardEvent
  convert = unsafeCoerce

pointer :: (PointerEvent -> Effect Unit) -> Cb
pointer f = cb $ f <<< convert
  where
  convert :: Web.Event -> PointerEvent
  convert = unsafeCoerce

type PointerEvent
  = { clientX :: Number
    , clientY :: Number
    }

className :: forall e. Attr e H.Class String => Array (String /\ Event Boolean) -> Event (Attribute e)
className = klass <<< classNameStr

classNameStr :: Array (String /\ Event Boolean) -> Event String
classNameStr =
  Array.foldl
    ( \acc (label /\ event) ->
        let
          f b s = if b then label <> " " <> s else s
        in
          lift2 f event acc
    )
    (pure "")

autoFocus :: forall e. Event (Attribute e)
autoFocus = pure $ unsafeAttribute { key: "autofocus", value: Prop' "" }

tabIndex :: forall e. Event Int -> Event (Attribute e)
tabIndex = map (\index -> unsafeAttribute { key: "tabIndex", value: Prop' $ show index })
