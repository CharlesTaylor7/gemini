module Gemini.Component.Puzzle
  ( component
  ) where

import Gemini.Prelude
import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.Point as Point
import Data.Set as Set
import Data.String.Common as String
import Data.Gemini (geminiLookup)
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Hooks (useState)
import Deku.Attributes (style)
import Gemini.Store as Store
import Gemini.Env (Env)
import Gemini.Component.Puzzle.Actions
import Gemini.DomInfo
import FRP.Event (sampleOnRight, filterMap)

type Props
  = { gemini :: Event Gemini
    , drag :: Store (Maybe Drag)
    , domInfo :: Effect DomInfo
    }

type DragProps = { drag :: Drag, domInfo :: DomInfo }
dragProps :: Props -> Event DragProps 
dragProps { drag, domInfo } =
  (Store.subscribe drag # filterMap identity) 
    `bindToEffect` \drag -> domInfo <#> { drag, domInfo: _ }


component :: Props -> Nut
component props = Deku.do
  D.div [ klass_ "gemini" ]
    $ Array.concat
        [ [ D.div [ klass_ "background" ] [] ]
        , map ringView inhabitants
        ]
  where
  ringView :: Ring -> Nut
  ringView ring =
    D.div [ klass_ $ ringClass ring ]
      $ disks ring

  disks :: Ring -> Array Nut
  disks ring =
    flip map inhabitants
      $ \position ->
          let
            location = Location { ring, position }
          in
            disk location props

disk :: Location -> Props -> Nut
disk location@(Location { position, ring }) props = Deku.do
  let
    color =
      props.gemini
        <#> geminiLookup location
        >>> _.color
        >>> show
        >>> String.toLower

    dragged = dragProps props <#> dragAngle ring

    initial = angleOnCircle position
  (pursx :: _ "<div ~diskAttrs~ />")
    ~~
      { diskAttrs:
          klass (append "disk " <$> color)
            <|> (style (diskStyle <$> (pure initial <|> (append initial <$> dragged))))
            --pure(diskStyle (angleOnCircle position))))
            <|> (D.OnPointerdown !:= pointer (onDragStart { drag: props.drag, location }))
      }

diskStyle :: Angle -> String
diskStyle diskAngle =
  let
    k = 43.0
    x = k * (1.0 + cosine diskAngle)
    y = k * (1.0 + sine diskAngle)
  in
    "left: " <> show x <> "%; top: " <> show y <> "%"

angleOnCircle :: forall n. Pos n => Cyclic n -> Angle
angleOnCircle k = turns <> -90.0 :* Degrees
  where
  turns = (Int.toNumber (unCyclic k)) / (Int.toNumber $ knownInt (proxy :: _ n)) :* Turns

ringClass :: Ring -> String
ringClass =
  const "gemini-ring "
    <> case _ of
        LeftRing -> "left"
        CenterRing -> "center"
        RightRing -> "right"

{-
hiddenLocations :: Set Location
hiddenLocations = ambiguousLocations # map _.alternate
-}
-- | angle of current ring being dragged, (via location that disambiguates)
dragAngle :: Ring -> DragProps -> Angle
dragAngle r {drag, domInfo} =
  if r == ring
  then angleEnd <> invert angleStart
  else mempty
  where
  Location { ring } = disambiguate drag
  angleWith point = do
    let origin = domInfo.ringCenter ring
    let p = point <> invert origin
    Point.angleToOrigin p

  angleStart = angleWith drag.initialPoint
  angleEnd = angleWith drag.currentPoint
