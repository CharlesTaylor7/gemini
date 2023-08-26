module Resize where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Web.DOM (Element)
import Partial.Unsafe (unsafeCrashWith)
import FRP.Event (Event, makeEvent)

foreign import data DomRect :: Type

onResize :: Element -> Event DomRect
onResize el = makeEvent ?callback
  --unsafeCrashWith ""
{- What I need is simply to attach an observer to a particular element
-}

{- full sketch of the API

foreign import data Observer :: Type
foreign import data Entry :: Type
type Callback = (EffectFn2 (Array Entry) Observer Unit)

foreign import newResizeObserver :: Callback -> Effect Observer
foreign import observe :: Observer -> Element -> Effect Unit
foreign import unobserve :: Observer -> Element -> Effect Unit
foreign import disconnect :: Observer -> Effect Unit
-}
