module Deku.Hooks.UseStore 
  ( Store
  , useStore
  , modify
  )
  where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Deku.Core (Nut)
import Deku.Extra (Event, Pusher)
import Deku.Hooks (useRef, useState)
import Deku.Do as Deku

type Store a = 
  { current :: Effect a
  , event :: Event a
  , dispatch :: Pusher a 
  }

modify :: forall a. Store a -> (a -> a) -> Effect Unit
modify { current, dispatch } transform =
  current >>= (transform >>> dispatch)

useStore 
  :: forall a
   . a
  -> (Store a -> Nut)
  -> Nut
useStore initial callback = Deku.do 
  dispatch /\ event <- useState initial
  current <- useRef initial event
  callback $ { current, event, dispatch }
