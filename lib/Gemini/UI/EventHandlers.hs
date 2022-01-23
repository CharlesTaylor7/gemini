module Gemini.UI.EventHandlers
  ( startDrag
  , onDrag
  , endDrag
  , mousePosition
  , RawEventHandler
  , ringClass
  , dragAngle
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


ringClass :: Ring -> Text
ringClass = \case
  LeftRing   -> "left"
  CenterRing -> "center"
  RightRing  -> "right"


type RawEventHandler m = RawNode -> RawEvent -> JSM (Continuation m Store)

getRingCenter :: Ring -> JSM Point
getRingCenter = fmap fst . getRingCoordinates

getRingCoordinates :: Ring -> JSM (Point, Double)
getRingCoordinates ring = do
  elem <- jsCall (jsg ("document" :: Text)) "querySelector" (".ring." <> ringClass ring)
  rect <- jsCall elem "getBoundingClientRect" ()
  w <- rect ! ("width" :: Text) >>= fromJSValUnchecked
  x <- rect ! ("left" :: Text) >>= fromJSValUnchecked
  y <- rect ! ("top" :: Text) >>= fromJSValUnchecked
  let r = w/2;
  pure $ (Point (x + r) (y + r), r)


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


startDrag :: Location -> RawEventHandler m
startDrag location _ event = do
  mouse <- mousePosition event
  pure $ Continuation.pur $
    (#drag ?~ DragState
      { location
      , initialPoint = mouse
      , currentPoint = mouse
      }
    )


onDrag :: MonadJSM m => RawEventHandler m
onDrag _ event = do
  -- origin <- getRingCenter ring
  mouse <- mousePosition event
  pure $ Continuation.pur $ updateDrag mouse

  {-
let distanceToRing :: Ring -> JSM (Double, Double)
              distanceToRing ring = do
                (origin, radius) <- getRingCoordinates ring
                let p = mouse ~~ origin
                pure $ (abs (norm p - radius), angle p)
-}


updateDrag :: Point -> Store -> Store
updateDrag = set $ #drag % _Just % #currentPoint


endDrag :: MonadJSM m => RawEventHandler m
endDrag _ event = do
  mouse <- mousePosition event
  pure $ Continuation.Pure $
    execState $ do
      modify $ updateDrag mouse
      drag <- use #drag
      case drag of
        Nothing -> pure ()
        Just drag -> do
          (#drag .= Nothing)
          -- TODO: reimplement
          {--
          let ring = drag ^. #location % #ring
          let theta = drag ^. #currentAngle
          let n = floor $ (theta / 20) + 0.5
          let direction = if signum n > 0 then Clockwise else AntiClockwise
          let motion = Motion { amount = abs n, rotation = Rotation { ring, direction } }
          modify $ applyMotionToStore motion
          --}

-- | angle of current ring being dragged
dragAngle :: Store -> Maybe (Ring, Double)
dragAngle = undefined

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
