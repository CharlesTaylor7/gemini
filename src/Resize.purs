module Resize
  ( DomRect
  , observe
  ) where

import Prelude

import Control.Monad.ST.Global (Global, toEffect)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as Ref
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, makeEvent)
import Web.DOM (Element)

-- | the listen callback, sets up the source of the event.
-- the event fires immediately after listening,
-- and whenever the element is resized inside the DOM
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
    -- Technically violates referential transparency, but the JS output is correct...
    ref = unsafePerformEffect $ toEffect $ Ref.new mempty
    ob = newResizeObserverF $ \resized -> do
      let el = NE.head resized
      pusher <- toEffect $ Ref.read ref
      pusher el.target

type Observer a =
  { ob :: ForeignObserver
  , ref :: STRef Global (a -> Effect Unit)
  }

foreign import data ForeignObserver :: Type

-- The callback only runs when an element resizes, which means the array is non empty
type Listener = NonEmptyArray { target :: Element } -> Effect Unit

foreign import data DomRect :: Type
foreign import getBoundingClientRectF :: Element -> Effect DomRect
foreign import newResizeObserverF :: Listener -> ForeignObserver
foreign import observeF :: Element -> ForeignObserver -> Effect Unit
foreign import disconnectF :: ForeignObserver -> Effect Unit

-- foreign import unobserve :: Element -> Observer -> Effect Unit
