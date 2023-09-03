module Deku.Extra
  ( autoFocus
  , tabIndex
  , keyboard
  , pointer
  , className
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

newtype ClassName = ClassName String

className :: forall e. Attr e H.Class String => Array (Event String) -> Event (Attribute e)
className = klass <<< coerce <<< foldMap safeCoerce
  where
    safeCoerce :: Event String -> Event ClassName
    safeCoerce = coerce

instance Semigroup ClassName where
  append a@(ClassName x) b@(ClassName y) 
    | x == "" = b
    | y == "" = a
    | otherwise =
      ClassName $
        x <> " " <> y

instance Monoid ClassName where
  mempty = ClassName ""


autoFocus :: forall e. Event (Attribute e)
autoFocus = pure $ unsafeAttribute { key: "autofocus", value: Prop' "" }

tabIndex :: forall e. Event Int -> Event (Attribute e)
tabIndex = map (\index -> unsafeAttribute { key: "tabIndex", value: Prop' $ show index })
