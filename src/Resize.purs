module Resize 
  ( DomRect
  , Observer
  , onResize
  , observer
  -- , subscribe
  -- , event
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



onResize :: Element -> Event DomRect
onResize el = makeEvent $ \pusher -> do

  pusher =<< getBoundingClientRect el

  ob <- newResizeObserver $ \_ -> 
    pusher =<< getBoundingClientRect el

  ob # observe el 

  pure $ ob # disconnect


observer :: Effect Observer
observer = do
  ref <- toEffect $ Ref.new mempty
  ob <- unsafePartial (
    newResizeObserver $ \[{ target }] -> do
      pusher <- toEffect $ Ref.read ref
      pusher =<< getBoundingClientRect target
  )
  pure $ Observer { ref, ob }

{-
subscribe :: Element -> Effect Observer -> Effect Unit
subscribe el ob = do
  ob <- ob
  ob # observe el


event :: Effect Observer -> Event DomRect 
event ob = makeEvent $ \_pusher -> do
  ob <- ob
  pure $ ob # disconnect
-}
   
newtype Observer = Observer
  { ob :: ForeignObserver
  , ref :: STRef Global (DomRect -> Effect Unit) 
  }

foreign import data ForeignObserver :: Type


type Listener = Array { target :: Element } -> Effect Unit

foreign import data DomRect :: Type
foreign import getBoundingClientRect :: Element -> Effect DomRect
foreign import newResizeObserver :: Listener -> Effect ForeignObserver
foreign import observe :: Element -> ForeignObserver -> Effect Unit
foreign import disconnect :: ForeignObserver -> Effect Unit
-- foreign import unobserve :: Element -> Observer -> Effect Unit
