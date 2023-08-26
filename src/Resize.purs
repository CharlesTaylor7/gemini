module Resize 
  ( DomRect
  , observe
  ) where

import Prelude
import Control.Monad.ST 
import Control.Monad.ST.Global
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as Ref
import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import FRP.Event (Event, makeEvent)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Web.DOM (Element)

observe :: { event :: Event DomRect, listen :: Element -> Effect Unit }
observe = { event: event ob, listen: subscribe ob } 
  where ob = observer

observer :: Effect Observer
observer = do
  ref <- toEffect $ Ref.new mempty
  ob <- unsafePartial (
    newResizeObserverF $ \[{ target }] -> do
      pusher <- toEffect $ Ref.read ref
      pusher =<< getBoundingClientRectF target
  )
  pure $ Observer { ref, ob }

subscribe :: Effect Observer -> Element -> Effect Unit
subscribe ob el = do
  Observer { ob } <- ob
  ob # disconnectF
  ob # observeF el

event :: Effect Observer -> Event DomRect 
event ob = makeEvent $ \pusher -> do
  Observer { ref, ob } <- ob
  _ <- toEffect $ Ref.write pusher ref
  pure $ ob # disconnectF
   

onResize :: Element -> Event DomRect
onResize el = makeEvent $ \pusher -> do

  pusher =<< getBoundingClientRectF el

  ob <- newResizeObserverF $ \_ -> 
    pusher =<< getBoundingClientRectF el

  ob # observeF el 

  pure $ ob # disconnectF


newtype Observer = Observer
  { ob :: ForeignObserver
  , ref :: STRef Global (DomRect -> Effect Unit) 
  }

foreign import data ForeignObserver :: Type


type Listener = Array { target :: Element } -> Effect Unit

foreign import data DomRect :: Type
foreign import getBoundingClientRectF :: Element -> Effect DomRect
foreign import newResizeObserverF :: Listener -> Effect ForeignObserver
foreign import observeF :: Element -> ForeignObserver -> Effect Unit
foreign import disconnectF :: ForeignObserver -> Effect Unit
-- foreign import unobserve :: Element -> Observer -> Effect Unit
