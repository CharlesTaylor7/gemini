module Resize
  ( DomRect
  , observe
  ) where

import Prelude
import Control.Monad.ST
import Control.Monad.ST.Global (Global, toEffect)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as Ref
import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, makeEvent)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Web.DOM (Element)

observe :: { event :: Event Element, listen :: Element -> Effect Unit }
observe =
  { event:
      makeEvent \pusher -> do
        let { ref, ob } = observer
        _ <- toEffect $ Ref.write pusher ref
        pure $ ob # disconnectF
  , listen:
      \el -> do
        let { ref, ob } = observer
        -- Unregister any previous elements listened to
        ob # disconnectF
        -- Fire once immediately
        pusher <- toEffect $ Ref.read ref
        pusher el
        -- Register resize listener
        ob # observeF el
  }

  where
  observer :: Observer Element
  observer = { ref, ob }
    where
    ref = unsafePerformEffect $ toEffect $ Ref.new mempty
    ob =
      unsafePartial
        ( newResizeObserverF
            $ \[ { target } ] -> do
                pusher <- toEffect $ Ref.read ref
                pusher target
        )

type Observer a
  = { ob :: ForeignObserver
    , ref :: STRef Global (a -> Effect Unit)
    }

foreign import data ForeignObserver :: Type

type Listener
  = Array { target :: Element } -> Effect Unit

foreign import data DomRect :: Type
foreign import getBoundingClientRectF :: Element -> Effect DomRect
foreign import newResizeObserverF :: Listener -> ForeignObserver
foreign import observeF :: Element -> ForeignObserver -> Effect Unit
foreign import disconnectF :: ForeignObserver -> Effect Unit

-- foreign import unobserve :: Element -> Observer -> Effect Unit
