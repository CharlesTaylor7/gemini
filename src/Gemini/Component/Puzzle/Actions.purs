module Gemini.Component.Puzzle.Actions
  ( onDragStart
  , onDragUpdate
  , onDragEnd
  , dragAngle
  , disambiguate
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
import Gemini.DomInfo (DomInfo)
import Gemini.Store as Store

onDragStart ::
  forall rest.
  { location :: Location
  , drag :: Store (Maybe Drag)
  | rest
  } ->
  PointerEvent ->
  Effect Unit
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

onDragUpdate ::
  forall rest.
  { drag :: Store (Maybe Drag)
  | rest
  } ->
  PointerEvent ->
  Effect Unit
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

onDragEnd ::
  forall rest.
  { drag :: Store (Maybe Drag)
  , gemini :: Store Gemini
  , domInfo :: Effect DomInfo
  | rest
  } ->
  PointerEvent ->
  Effect Unit
onDragEnd props@{ drag: dragS, gemini } event = do
  onDragUpdate props event
  drag <- Store.read dragS
  case drag of
    Nothing -> pure unit
    Just drag -> do
      angle <- dragAngle props
      let Location { ring } = disambiguate drag
      let n = angleToPosition angle
      let motion = Motion { amount: n, ring }
      Store.set dragS Nothing
      Store.modify gemini $ Gemini.applyToGemini motion

point :: PointerEvent -> Point
point { clientX, clientY } = Point { x: clientX, y: clientY }

angleToPosition :: forall n. Pos n => Angle -> Cyclic n
angleToPosition angle = cyclic $ Int.floor $ (k * turns) + 0.5
  where
  k = Int.toNumber $ knownInt (proxy :: _ n)
  turns = angle `Angle.as` Angle.Turns

-- | angle of current ring being dragged, (via location that disambiguates)
dragAngle ::
  forall rest.
  { drag :: Store (Maybe Drag)
  , domInfo :: Effect DomInfo
  | rest
  } ->
  Effect Angle
dragAngle { drag, domInfo } = do
  maybeDrag <- Store.read drag
  case maybeDrag of
    Nothing -> pure mempty
    Just drag@{ initialPoint, currentPoint } -> do
      let Location { ring } = disambiguate drag
      angleStart <- angleWith ring initialPoint
      angleEnd <- angleWith ring currentPoint
      pure $ angleEnd <> invert angleStart
  where
  angleWith :: Ring -> Point -> Effect Angle
  angleWith ring point = do
    origin <- ringOrigin domInfo ring
    let p = point <> invert origin
    pure $ Point.angleToOrigin p

ringOrigin :: Effect DomInfo -> Ring -> Effect Point
ringOrigin domInfo ring =
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
