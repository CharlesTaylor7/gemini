module Gemini.Store
  ( Store
  , useStore
  , modify
  , set
  , subscribe
  , read
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Core (Nut)
import Deku.Do as Deku
import Deku.Extra (Event, Pusher)
import Deku.Hooks (useRef, useState, useState')
import Effect (Effect)
import Utils

modify :: forall a. Store a -> (a -> a) -> Effect Unit
modify (Store { ref, pusher }) transform =
  ref >>= (transform >>> pusher)

set :: forall a. Store a -> a -> Effect Unit
set (Store { pusher }) value =
  pusher value

subscribe :: forall a. Store a -> Event a
subscribe (Store { event }) = event

read :: forall a. Store a -> Effect a
read (Store { ref }) = ref

useStore ::
  forall a.
  a ->
  (Store a -> Nut) ->
  Nut
useStore initial callback = Deku.do
  pusher /\ event <- useState initial
  ref <- useRef initial event
  callback $ Store { event, ref, pusher }

newtype Store a
  = Store
  { event :: Event a
  , pusher :: a -> Effect Unit
  , ref :: Effect a
  }
