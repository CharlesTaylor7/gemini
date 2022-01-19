module Gemini.UI.EventHandlers
  ( startDrag
  , onDrag
  , endDrag
  , mousePosition
  , RawEventHandler
  ) where

import           Relude

import           Data.Permutation
import           Language.Javascript.JSaddle (JSVal, MakeArgs, ToJSVal (..), fromJSValUnchecked, instanceOf, jsg, (!!),
                                              (!), (#))
import           Optics                      hiding ((#))
import           Optics.State.Operators
import           Shpadoinkle                 hiding (text)
import qualified Shpadoinkle.Continuation    as Continuation

import           Gemini.Types
import           Gemini.UI.Actions


type RawEventHandler m = RawNode -> RawEvent -> JSM (Continuation m Store)

getGeminiOrigin :: JSM Point
getGeminiOrigin = do
  elem <- jsCall (jsg ("document" :: Text)) "getElementsByClassName" ("gemini" :: Text) >>= (!! 0)
  rect <- jsCall elem "getBoundingClientRect" ()
  x <- rect ! ("left" :: Text) >>= fromJSValUnchecked
  y <- rect ! ("top" :: Text) >>= fromJSValUnchecked
  pure $ Point x y


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


startDrag :: Ring -> RawEventHandler m
startDrag ring _ event = do
  origin <- getGeminiOrigin
  mouse <- mousePosition event
  pure $ Continuation.pur $
    (#drag ?~ DragState
      { ring
      , initialAngle = angle (mouse ~~ origin ~~ ringCenter ring)
      , currentAngle = 0
      }
    )


onDrag :: RawEventHandler m
onDrag _ event = do
  origin <- getGeminiOrigin
  mouse <- mousePosition event
  pure $ Continuation.Pure $ updateDrag (mouse ~~ origin)


updateDrag :: Point -> Store -> Store
updateDrag mouse = (#drag % _Just) %~ \drag -> do
  let origin = drag ^. #ring % to ringCenter
  let pointRelativeToRing = mouse ~~ origin
  drag & #currentAngle .~ (angle pointRelativeToRing - initialAngle drag)


endDrag :: RawEventHandler m
endDrag _ event = do
  origin <- getGeminiOrigin
  mouse <- mousePosition event
  pure $ Continuation.Pure $
    execState $ do
      modify $ updateDrag (mouse ~~ origin)
      drag <- use #drag
      case drag of
        Nothing -> pure ()
        Just drag -> do
          (#drag .= Nothing)
          let ring = drag ^. #ring
          let theta = drag ^. #currentAngle
          let n = floor $ (theta / 20) + 0.5
          let direction = if signum n > 0 then Clockwise else AntiClockwise
          let motion = Motion { amount = abs n, rotation = Rotation { ring, direction } }
          modify $ applyMotionToStore motion


ringCenter :: Ring -> Point
ringCenter = \case
  LeftRing   -> Point 150 150
  CenterRing -> Point (450 - 135) 150
  RightRing  -> Point (750 - 270) 150


-- dimensions
-- math utils
toDegrees :: Double -> Double
toDegrees th = (th * 180) / pi

angle :: Point -> Double
angle (Point x y) = atan2 y x & toDegrees


jsCall :: (ToJSVal js, MakeArgs args) => js -> Text -> args -> JSM JSVal
jsCall js method args = toJSVal js # method $ args

jsConsoleLog :: JSVal -> JSM ()
jsConsoleLog text = void $ jsg ("console" :: Text) # ("log" :: Text) $ text
