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

import           Data.Cyclic
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
ringClass = const "ring " <> \case
  LeftRing   -> "left"
  CenterRing -> "center"
  RightRing  -> "right"


type RawEventHandler m a = RawNode -> RawEvent -> JSM (Continuation m a)

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


startDrag :: Location -> RawEventHandler m Store
startDrag location _ event = do
  mouse <- mousePosition event
  pure $ Continuation.pur $
    (#drag ?~ DragState
      { location = dragRing location
      , initialPoint = mouse
      , currentPoint = mouse
      }
    )


onDrag :: MonadJSM m => RawEventHandler m Store
onDrag _ event = do
  -- origin <- getRingCenter ring
  mouse <- mousePosition event
  pure $ Continuation.pur $ updateDrag mouse




updateDrag :: Point -> Store -> Store
updateDrag = set $ #drag % _Just % #currentPoint


endDrag :: MonadJSM m => RawEventHandler m Store
endDrag _ event = do
  mouse <- mousePosition event
  pure $ Continuation.Pure $
    execState $ do
      modify $ updateDrag mouse
      drag <- dragAngle <$> get
      case drag of
        Nothing -> pure ()
        Just (location, theta) -> do
          (#drag .= Nothing)
          let ring = location ^. #ring
          let n = angleToPosition theta
          let motion = Motion { amount = abs n, rotation = Rotation { ring, direction = Clockwise } }
          case normalize motion of
            Nothing     -> pure ()
            Just motion -> modify $ applyMotionToStore motion


angleToPosition :: forall n. KnownNat n => Double -> Cyclic n
angleToPosition theta = cyclic $ floor $ (k * theta) / 360 + 0.5
  where k = fromIntegral $ knownInt @n


disambiguate :: DragState -> Location
disambiguate = undefined


-- | angle of current ring being dragged, (via location that disambiguates)
dragAngle :: Store -> Maybe (Location, Double)
dragAngle store =
  case store ^. #drag of
    Nothing -> Nothing
    Just drag -> do
      let location :: Location
          location = case drag ^. #location of
            Obvious location -> location
            Ambiguous loc1 loc2 -> do
              let distanceTo :: Location -> Double
                  distanceTo loc = do
                    let radius = store ^. #dom ^. #ringRadius
                    let Just origin = store ^? #dom % #ringCenters % ix (loc ^. #ring)
                    let mouse = drag ^. #currentPoint
                    let p = mouse ~~ origin
                    abs (norm p - radius)
              if distanceTo loc1 <= distanceTo loc2 then loc1 else loc2

      let angleWith :: Point -> Double
          angleWith point = do
            let radius = store ^. #dom ^. #ringRadius
            let Just origin = store ^? #dom % #ringCenters % ix (location ^. #ring)
            let mouse = drag ^. #currentPoint
            let p = point ~~ origin
            angle p

      let DragState { initialPoint, currentPoint } = drag
      let currentAngle = angleWith currentPoint - angleWith initialPoint
      Just $ (location, currentAngle)



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
