module Deku.Hooks.UseStore
  ( Store
  , useStore
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Deku.Core (Nut)
import Deku.Extra (Event, Pusher)
import Deku.Hooks (useRef, useState, useState')
import Deku.Do as Deku
import Gemini.Prelude (logAnything)

type Store a
  = { subscribe :: Event a
    , modify :: (a -> a) -> Effect Unit
    , ref :: Effect a
    }

modify :: forall a. Effect a -> Pusher a -> (a -> a) -> Effect Unit
modify ref pusher transform = do
  ref >>= (transform >>> pusher)
  ref >>= logAnything "store"

useStore ::
  forall a.
  a ->
  (Store a -> Nut) ->
  Nut
useStore initial callback = Deku.do
  pusher /\ event <- useState initial
  ref <- useRef initial event
  callback $ { subscribe: event, modify: modify ref pusher, ref }
