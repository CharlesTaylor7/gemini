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
import Deku.Control ((<#~>))
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Pursx (pursx, (~~), (~!~))
import Deku.Attributes (klass_, klass,  href_)
import Deku.Attribute (xdata, (!:=))
import Deku.Attribute as Attr
import Deku.Hooks.UseStore (Store, useStore)
import Deku.Extra (Event, className)

import Gemini.Env (Env)


component :: Store AppState -> Nut
component store = Deku.do
  D.div [ klass_ "gemini" ] $
    Array.concat 
      [ [D.div [klass_ "background"] [] ]
      , map ringView inhabitants
      ]
  where
    ringView :: Ring -> Nut
    ringView ring =
      D.div [ klass_ $ ringClass ring] $
        disks ring

    disks :: Ring -> Array Nut
    disks ring = flip map inhabitants $ \position ->
      let
        location = Location { ring, position }
      in
        disk location (store.subscribe <#> _.gemini)
        

disk :: Location -> Event Gemini -> Nut
disk location@(Location { position }) gemini = 
  let
    color =
      gemini
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
          , D.Style !:= fold [ "left: ", show x, "%; top: ", show y, "%"]
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
ringClass = const "gemini-ring " <> case _ of
  LeftRing   -> "left"
  CenterRing -> "center"
  RightRing  -> "right"



{-
loadDomInfo :: JSM DomInfo
loadDomInfo = do
  ringInfo :: [(Ring, Point)] <- for inhabitants $ \ring -> do
    let selector = ringClass ring # String.words # String.intercalate "." # ("." <>)
    elem <- jsCall (jsg ("document" :: String)) "querySelector" selector
    rect <- jsCall elem "getBoundingClientRect" ()
    width <- rect ! ("width" :: String) >>= fromJSValUnchecked
    left <- rect ! ("left" :: String) >>= fromJSValUnchecked
    top <- rect ! ("top" :: String) >>= fromJSValUnchecked
    let radius = width / 2;
    pure $ (ring, Point (left + radius) (top + radius))

  let getDiameter :: String -> JSM Double
      getDiameter selector = do
        elem <- jsCall (jsg ("document" :: String)) "querySelector" selector
        rect <- jsCall elem "getBoundingClientRect" ()
        rect ! ("width" :: String) >>= fromJSValUnchecked

  ringRadius <- do
    ring <- getDiameter ".ring"
    disk <- getDiameter ".disk"
    pure $ (ring - disk) / 2

  pure $
    DomInfo
      { ringRadius
      , ringCenters = ringInfo # Map.fromList
      }

-}
