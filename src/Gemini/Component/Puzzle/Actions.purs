module Gemini.Component.Puzzle.Actions
  ( onDragStart
  , onDragUpdate
  , onDragEnd
  ) where

import Gemini.Prelude
import Deku.Control (text, text_)
import Deku.Core (Nut, NutWith)
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Pursx (pursx, (~~), (~!~))
import Deku.Attributes (klass_, href_)
import Deku.Attribute (xdata, (!:=))
import Deku.Attribute as Attr
import Deku.Hooks (useState)
import Deku.Hooks.UseStore (Store)
import Deku.Listeners as Listener
import Deku.Extra (className)
import Data.Array as Array
import Data.Unfoldable
import Data.Gemini as Gemini
import Data.Gemini.Motions (l, l', c, c', r, r')
import Web.Event.Internal.Types as Web
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

type DragStore
  = Store (Maybe Drag)

onDragStart :: { drag :: DragStore, location :: Location } -> Web.Event -> Effect Unit
onDragStart { drag, location } event =
  drag.modify \_ ->
    Just
      { location: Gemini.dragRing location
      , chosen: Nothing
      , initialPoint: mouse
      , currentPoint: mouse
      }

  where
  mouse = unsafePointerEvent >>> point $ event

onDragUpdate :: DragStore -> Web.Event -> Effect Unit
onDragUpdate drag event = do
  maybe <- drag.ref
  case maybe of
    Nothing -> pure unit
    Just {} -> do
      -- | todo: disambiguate ambiguous
      drag.modify
        $ underMaybe
        $ \drag@{ location, chosen, initialPoint, currentPoint } ->
            drag
              { currentPoint = mouse
              }

  where
  mouse = unsafePointerEvent >>> point $ event

onDragEnd :: { drag :: DragStore, gemini :: Store Gemini } -> Web.Event -> Effect Unit
onDragEnd { drag, gemini } event = pure unit

underMaybe :: forall a. (a -> a) -> (Maybe a -> Maybe a)
underMaybe _ Nothing = Nothing
underMaybe f (Just a) = Just (f a)

-- Point event utilities
type PointerEvent
  = { clientX :: Number
    , clientY :: Number
    }

-- given a mouse event / pointer event, gets the location of the event
unsafePointerEvent :: Web.Event -> PointerEvent
unsafePointerEvent = unsafeCoerce

point :: PointerEvent -> Point
point { clientX, clientY } = Point { x: clientX, y: clientY }


{-


updateDrag :: Point -> Store -> Store
updateDrag mouse = execState $ do

  -- update current point to where mouse is
  (#drag % _Just % #currentPoint .= mouse)

  initialLocation <- preuse $ #drag % _Just % #location
  dragged <- get <&> dragAngle
  chosen <- preuse $ #drag % _Just % #chosen

  case (initialLocation, dragged) of
    (Just (Ambiguous left _), Just (loc, Turns turns)) ->
      case chosen of
        -- lock choice
        Nothing | abs (turns * 18) > 1 ->
          (#drag % _Just % #chosen ?= if loc == left then ChoseLeft else ChoseRight)

        -- unlock choice
        Just _  | abs (turns * 18) < 1 ->
          (#drag % _Just % #chosen .= Nothing)

        -- do nothing
        _ -> pure ()

    _ -> pure ()


endDrag :: MonadJSM m => Point -> Action m ()
endDrag mouse = do
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
        Just motion -> applyMotionToStore motion


angleToPosition :: forall n. NonZero n => Angle -> Cyclic n
angleToPosition (Turns turns) = Cyclic $ floor $ (k * turns) + 0.5
  where k = fromIntegral $ knownInt @n


disambiguate :: Store -> DragState -> Location
disambiguate store drag =
  case drag ^. #location of
    Obvious location -> location
    Ambiguous loc1 loc2 ->
      case drag ^. #chosen of
        Just ChoseLeft -> loc1
        Just ChoseRight -> loc2
        Nothing -> do
          let distanceTo :: Location -> Double
              distanceTo location = do
                let radius = store ^. #dom ^. #ringRadius
                let origin = ringOrigin store (location ^. #ring)
                let mouse = drag ^. #currentPoint
                let p = mouse ~~ origin
                abs (norm p - radius)
          if distanceTo loc1 <= distanceTo loc2 then loc1 else loc2


ringOrigin :: Store -> Ring -> Point
ringOrigin store ring =
  case store ^? #dom % #ringCenters % ix ring of
    Just origin -> origin
    _           -> error "impossible"

-- | angle of current ring being dragged, (via location that disambiguates)
dragAngle :: Store -> Maybe (Location, Angle)
dragAngle store =
  case store ^. #drag of
    Nothing -> Nothing
    Just drag -> do
      let location = disambiguate store drag
      let angleWith :: Point -> Angle
          angleWith point = do
            let origin = ringOrigin store (location ^. #ring)
            let p = point ~~ origin
            angleToOrigin p

      let DragState { initialPoint, currentPoint } = drag
      let currentAngle = angleWith currentPoint ~~ angleWith initialPoint
      Just $ (location, currentAngle)
-}
