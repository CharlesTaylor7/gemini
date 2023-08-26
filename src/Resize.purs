module Resize 
  ( onResize
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import FRP.Event (Event, makeEvent)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Web.DOM (Element)


type Subscribe = (DomRect -> Effect Unit) -> Effect (Effect Unit)

onResize :: Element -> Event DomRect
onResize el = makeEvent $ \pusher -> do

  pusher =<< getBoundingClientRect el

  ob <- newResizeObserver $ \_ -> 
    pusher =<< getBoundingClientRect el

  ob # observe el 

  pure $ ob # disconnect
    

foreign import data Observer :: Type


type Listener = Array { target :: Element } -> Effect Unit

foreign import data DomRect :: Type
foreign import getBoundingClientRect :: Element -> Effect DomRect
foreign import newResizeObserver :: Listener -> Effect Observer
foreign import observe :: Element -> Observer -> Effect Unit
foreign import disconnect :: Observer -> Effect Unit
-- foreign import unobserve :: Element -> Observer -> Effect Unit
