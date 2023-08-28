module Gemini.Component.Puzzle
  ( component
  ) where

import Gemini.Prelude
import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.Set as Set
import Data.String.Common as String
import Data.Gemini (geminiLookup)
import Deku.Control (text, text_)
import Deku.Listeners as Event
import Deku.Core (Nut, NutWith)
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Pursx (pursx, (~~), (~!~))
import Deku.Attributes (klass_, klass, href_)
import Deku.Attribute (xdata, (!:=))
import Deku.Attribute as Attr
import Gemini.Store as Store 
import Deku.Extra (Event, className)
import Gemini.Env (Env)
import Gemini.Component.Puzzle.Actions

type Props
  = { gemini :: Store Gemini, drag :: Store (Maybe Drag) }

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
disk location@(Location { position }) props =
  let
    color =
      Store.subscribe props.gemini
        <#> geminiLookup location
        >>> _.color
        >>> show
        >>> String.toLower

    diskAngle = angleOnCircle position

    k = 43.0
    x = k * (1.0 + cosine diskAngle)
    y = k * (1.0 + sine diskAngle)
  in
    D.div
      [ klass (map (append "disk ") color)
      --, D.Style !:= (fold [ "left: ", show x, "%; top: ", show y, "%" ])
      , D.OnPointerdown !:= pointer (onDragStart { drag: props.drag, location })
      ]
      []
{-

hiddenLocations :: Set Location
hiddenLocations = ambiguousLocations # map _.alternate

-}
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
