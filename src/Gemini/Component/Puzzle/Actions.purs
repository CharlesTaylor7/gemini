module Gemini.Component.Puzzle.Actions
  ( onDragStart
  , onDragUpdate
  , onDragEnd
  ) where

import Gemini.Prelude
import Deku.Do as Deku
import Data.Array as Array
import Data.Unfoldable
import Data.Int as Int
import Data.Angle as Angle
import Data.Gemini as Gemini
import Data.Point as Point
import Data.Gemini.Motions (l, l', c, c', r, r')
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Gemini.DomInfo
import Gemini.Store as Store

type Props
  = ( domInfo :: Effect DomInfo, drag :: Store (Maybe Drag), gemini :: Store Gemini )

onDragStart :: { location :: Location, drag :: Store (Maybe Drag) } -> PointerEvent -> Effect Unit
onDragStart { drag, location } event =
  Store.set drag
    $ Just
        { location: Gemini.dragRing location
        , chosen: Nothing
        , initialPoint: mouse
        , currentPoint: mouse
        }

  where
  mouse = point event

onDragUpdate :: { | Props } -> PointerEvent -> Effect Unit
onDragUpdate { drag } event = do
  maybe <- Store.read drag
  case maybe of
    Nothing -> pure unit
    -- | todo: disambiguate ambiguous
    Just d -> do
      Store.set drag
        $ Just
        $ d
            { currentPoint = mouse
            }

  where
  mouse = point event

onDragEnd :: { | Props } -> PointerEvent -> Effect Unit
onDragEnd { drag, gemini } event = do
  pure unit

  where
  mouse = point $ event


point :: PointerEvent -> Point
point { clientX, clientY } = Point { x: clientX, y: clientY }

angleToPosition :: forall n. Pos n => Angle -> Cyclic n
angleToPosition angle = cyclic $ Int.floor $ (k * turns) + 0.5
  where
  k = Int.toNumber $ knownInt (proxy :: _ n)
  turns = angle `Angle.as` Angle.Turns

{-
-- | angle of current ring being dragged, (via location that disambiguates)
dragAngle :: Store -> { location :: Location, angle :: Angle }
dragAngle store = do
  let location = disambiguate store
  let angleWith :: Point -> Angle
      angleWith point = do
        let origin = ringOrigin store (location.ring)
        let p = point <> invert origin
        Point.angleToOrigin p

  let { initialPoint, currentPoint } = drag
  let currentAngle = angleWith currentPoint <> invert (angleWith initialPoint)
  {location, angle: currentAngle }
  -}
ringOrigin :: { | Props } -> Ring -> Effect Point
ringOrigin { domInfo } ring =
  domInfo <#> \dom -> dom.ringCenter ring

disambiguate :: Drag -> Location
disambiguate drag = do
  case drag.location of
    Obvious location -> location
    Ambiguous loc1 loc2 ->
      case drag.chosen of
        Just ChoseLeft -> loc1
        Just ChoseRight -> loc2
        -- | left wins
        Nothing -> loc1

--unsafeCrashWith "Todo"
{-
          do
          let distanceTo :: Location -> Double
              distanceTo location = do
                let radius = store ^. #dom ^. #ringRadius
                let origin = ringOrigin store (location ^. #ring)
                let mouse = drag ^. #currentPoint
                let p = mouse ~~ origin
                abs (norm p - radius)
          if distanceTo loc1 <= distanceTo loc2 then loc1 else loc2
          -}
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
-}
