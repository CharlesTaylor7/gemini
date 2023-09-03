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
import Data.Gemini (geminiLookup, locationToIndex, location)
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Hooks (useState)
import Deku.Attributes (style)
import Gemini.Store as Store
import Gemini.Env (Env)
import Gemini.Component.Puzzle.Actions (disambiguate, onDragStart)
import Gemini.DomInfo (DomInfo, bindToEffect)
import FRP.Event (sampleOnRight, filterMap)

type Props
  = { gemini :: Event Gemini
    , drag :: Store (Maybe Drag)
    , domInfo :: Effect DomInfo
    }

type DragProps
  = { drag :: Maybe Drag
    , domInfo :: DomInfo
    }

dragProps :: Props -> Event DragProps
dragProps { drag, domInfo } =
  (Store.subscribe drag)
    `bindToEffect`
      \drag -> domInfo <#> { drag, domInfo: _ }

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
disk location@(Location { position, ring }) props =
  (pursx :: _ "<div ~diskAttrs~ />")
    ~~
      { diskAttrs:
          klass (append "disk " <$> color)
            <|> (style (diskStyle <$> (pure initial <|> (append initial <$> dragged))))
            <|> (D.OnPointerdown !:= pointer (onDragStart { drag: props.drag, location }))
      }
  where
  color =
    props.gemini
      <#> geminiLookup location
      >>> _.color
      >>> show
      >>> String.toLower

  dragged = dragProps props <#> dragAngle ring

  initial = angleOnCircle position

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


hiddenLocationIndices :: Drag -> Set Int
hiddenLocationIndices drag = 
  case ring of
    LeftRing -> leftRingHides
    CenterRing -> centerRingHides
    RightRing -> rightRingHides
  where
    Location {ring} = disambiguate drag 

leftRingHides :: Set Int
leftRingHides = Set.fromFoldable $ map locationToIndex 
  [ location CenterRing 16 
  , location CenterRing 11 
  ]

centerRingHides :: Set Int
centerRingHides = Set.fromFoldable $ map locationToIndex 
  [ location LeftRing 2
  , location LeftRing 7
  , location RightRing 16
  , location RightRing 11
  ]

rightRingHides :: Set Int
rightRingHides = Set.fromFoldable $ map locationToIndex 
  [ location CenterRing 2
  , location CenterRing 7
  ]


-- | angle of current ring being dragged, (via location that disambiguates)
dragAngle :: Ring -> DragProps -> Angle
dragAngle r { drag, domInfo } = fromMaybe mempty do
  drag <- drag

  let Location { ring } = disambiguate drag
  guard $ r == ring

  let offset = invert $ domInfo.ringCenter ring
  let angleWith = append offset >>> Point.angleToOrigin
  pure $ angleWith drag.currentPoint <> invert (angleWith drag.initialPoint)
