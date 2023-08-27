module Deku.Extra
  ( className
  , autoFocus
  , tabIndex
  , Pusher
  , module FRP.Event
  ) where

import Prelude
import Effect (Effect)
import FRP.Event (Event)
import FRP.Event as FRP.Event
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Control.Apply (lift2)
import Deku.DOM as H
import Deku.Attribute (Attribute, class Attr, unsafeAttribute, AttributeValue(..))
import Deku.Attributes (klass)

type Pusher a
  = a -> Effect Unit

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
