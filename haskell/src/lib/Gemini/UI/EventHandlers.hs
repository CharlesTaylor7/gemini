module Gemini.UI.EventHandlers
  ( onDragStart
  , onDragUpdate
  , onDragEnd
  , Prop
  ) where

import           Relude

import           Data.Point

import           Shpadoinkle              hiding (text)
import qualified Shpadoinkle.Continuation as Continuation

import           Gemini.FFI               (fromJSValUnchecked, instanceOf, jsg, toJSVal, (!!), (!))
import           Gemini.Types
import           Gemini.UI.Actions        as Actions


mousePosition :: RawEvent -> JSM Point
mousePosition event = do
  isMouseEvent <- event `instanceOf` jsg ("MouseEvent"::Text)
  touchOrMouseEvent <-
      case isMouseEvent of
        True  -> toJSVal event
        False -> toJSVal event ! ("changedTouches" :: Text) !! 0

  x <- touchOrMouseEvent ! ("clientX" :: Text) >>= fromJSValUnchecked
  y <- touchOrMouseEvent ! ("clientY" :: Text) >>= fromJSValUnchecked

  pure $ Point x y


onDragStart :: Location -> Prop m Store
onDragStart location = listenerProp $ \_ event -> do
  mouse <- mousePosition event
  pure $ Continuation.pur $ startDrag location mouse


onDragUpdate :: MonadJSM m => Prop m Store
onDragUpdate = listenerProp $ \_ event -> do
  mouse <- mousePosition event
  pure $ Continuation.pur $ updateDrag mouse


onDragEnd :: MonadJSM m => Prop m Store
onDragEnd = listenerProp $ \_ event -> do
  mouse <- mousePosition event
  pure $ Actions.run $ Actions.endDrag mouse
