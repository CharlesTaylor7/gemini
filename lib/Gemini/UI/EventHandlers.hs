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

import           Data.Angle
import           Data.Cyclic
import           Data.Permutation
import           Data.Point

import           Optics
import           Optics.State.Operators
import           Shpadoinkle              hiding (text)
import qualified Shpadoinkle.Continuation as Continuation

import           Gemini.Jsaddle
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
      , chosen = Nothing
      , initialPoint = mouse
      , currentPoint = mouse
      }
    )


onDrag :: MonadJSM m => RawEventHandler m Store
onDrag _ event = do
  mouse <- mousePosition event
  pure $ Continuation.pur $ updateDrag mouse


updateDrag :: Point -> Store -> Store
updateDrag point = execState $ do
  (#drag % _Just % #currentPoint .= point)
  dragged <- get <&> dragAngle
  -- drag <- use #drag
  chosen <- preuse $ #drag % _Just % #chosen
  location <- preuse $ #drag % _Just % #location

  case (dragged, chosen, location) of
    (Just (loc, Turns turns), Nothing, Just (Ambiguous left right))
      | abs (turns * 18) > 1 -> do

        (#drag % _Just % #chosen ?= if loc == left then L else R)

    (Just (loc, Turns turns), Just _, _)
      | abs (turns * 18) < 1 -> do

        (#drag % _Just % #chosen .= Nothing)

    _ -> pure ()


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


angleToPosition :: forall n. KnownNat n => Angle -> Cyclic n
angleToPosition (Turns turns) = Cyclic $ floor $ (k * turns) + 0.5
  where k = fromIntegral $ knownInt @n


disambiguate :: DragState -> Location
disambiguate = undefined


-- | angle of current ring being dragged, (via location that disambiguates)
dragAngle :: Store -> Maybe (Location, Angle)
dragAngle store =
  case store ^. #drag of
    Nothing -> Nothing
    Just drag -> do
      let location :: Location
          location = case drag ^. #location of
            Obvious location -> location
            Ambiguous loc1 loc2 -> do
              case drag ^. #chosen of
                Just L -> loc1
                Just R -> loc2
                Nothing -> do
                  let distanceTo :: Location -> Double
                      distanceTo location = do
                        let radius = store ^. #dom ^. #ringRadius
                        let Just origin = store ^? #dom % #ringCenters % ix (location ^. #ring)
                        let mouse = drag ^. #currentPoint
                        let p = mouse ~~ origin
                        abs (norm p - radius)
                  if distanceTo loc1 <= distanceTo loc2 then loc1 else loc2

      let angleWith :: Point -> Angle
          angleWith point = do
            let Just origin = store ^? #dom % #ringCenters % ix (location ^. #ring)
            let mouse = drag ^. #currentPoint
            let p = point ~~ origin
            angleToOrigin p

      let DragState { initialPoint, currentPoint } = drag
      let currentAngle = angleWith currentPoint ~~ angleWith initialPoint
      Just $ (location, currentAngle)
