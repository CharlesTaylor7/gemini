module Deku.Hooks.Extra 
  ( Store
  , useStore
  )
  where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Deku.Core (Nut)
import Deku.Extra (Event, Pusher)
import Deku.Hooks (useRef, useState')
import Deku.Do as Deku

type Store a = { value :: Effect a, dispatch :: Pusher a }

useStore 
  :: forall a
   . a
  -> (Store a -> Nut)
  -> Nut
useStore initial callback = Deku.do 
  pusher /\ event <- useState'
  effect <- useRef initial event
  callback $ { value: effect, dispatch: pusher }
