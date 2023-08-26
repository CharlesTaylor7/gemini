module Resize where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Web.DOM (Element)

foreign import data Observer :: Type
foreign import data Entry :: Type
type Callback = (EffectFn2 (Array Entry) Observer Unit)

foreign import newResizeObserver :: Callback -> Effect Observer
foreign import observe :: Observer -> Element -> Effect Unit
foreign import unobserve :: Observer -> Element -> Effect Unit
foreign import disconnect :: Observer -> Effect Unit
