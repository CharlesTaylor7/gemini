module Deku.Hooks.Extra where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Deku.Core (Nut(..), NutF(..), envy)
import Deku.Extra (Event, Pusher)
import Deku.Hooks (useRef, useState)
import Deku.Do as Deku

useStateRef 
  :: forall a
   . a
  -> ((Effect a /\ Pusher a) -> Nut)
  -> Nut
useStateRef initial callback = Deku.do 
  pusher /\ event <- useState initial
  effect <- useRef initial event
  callback $ effect /\ pusher

