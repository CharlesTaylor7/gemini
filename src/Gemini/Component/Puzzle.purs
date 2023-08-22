module Gemini.Component.Puzzle
  ( component
  ) where

import Prelude
import Data.Tuple.Nested ((/\))
import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Effect.Console (log)

import Deku.Control (text, text_)
import Deku.Listeners as Event
import Deku.Core (Nut, NutWith)
import Deku.Do as Deku
import Deku.DOM as D
import Deku.Pursx (pursx, (~~), (~!~))
import Deku.Attributes (klass_, href_)
import Deku.Attribute (xdata, (!:=))
import Deku.Attribute as Attr
import Deku.Hooks (useState)
import Deku.Extra (className)

import Gemini.Env (Env)
import Gemini.Types (Gemini)


component :: Gemini -> Nut
component = Deku.do
  D.div
    [ klass_ "gemini" ]
    $
    concat
      [ [D.div [klass_ "background"] [] ]
      , map ringView inhabitants
      ]
  where
    hidden :: Set Location
    hidden = hiddenLocations store

    ringView :: Ring -> Nut
    ringView ring =
      D.div [ D.className $ ringClass ring] $
        disks ring

    disks :: Ring -> Array Nut
    disks ring = flip map inhabitants $ \position ->
      let
        location = Location ring position
        (color, diskLabel) =
          case gemini ^? geminiIx location of
            Just Disk { color, label } -> (String.toLower $ show color, show label)
            Nothing                    -> ("unknown", "")


        diskAngle = angleOnCircle position

        k = 43
        (x, y) =
          ( k * (1 + cosine diskAngle)
          , k * (1 + sine diskAngle)
          )

        defaultLabel :: Maybe String
        defaultLabel = (options ^. #showLabels && isn't (#hover % _Just) store) `orNothing` diskLabel

        toLabelSpan label = D.span [ ("className", "disk-label") ] [ D.text label ]
      in
          D.div
            ( D.class'
              [ ("disk", True)
              , (color, True)
              , ("dragging", isDraggedDisk location)
              , ("highlight", highlighted ^. contains location)
              , ("active-cycle", activeCycle ^. contains location)
              , ("hidden", hidden ^. contains location)
              ]
            : D.styleProp
              [ ("left", show x <> "%")
              , ("top", show y <> "%")
              ]
            : [ ("mousedown", onDragStart location)
              , ("touchstart", onDragStart location)
              ]
            )
            []


hiddenLocations :: Set Location
hiddenLocations =
  ambiguousLocations
  & map _.alternate


angleOnCircle :: forall n. Pos n => Cyclic n -> Angle
angleOnCircle (Cyclic k) = turns ~~ offset
  where
    turns = Turns $ (fromIntegral k) / (fromIntegral $ knownInt @n)
    -- By arbitrary choice, the initial position is at the top of the circle
    offset = Degrees 90


ringClass :: Ring -> String
ringClass = const "ring " <> \case
  LeftRing   -> "left"
  CenterRing -> "center"
  RightRing  -> "right"



{-
loadDomInfo :: JSM DomInfo
loadDomInfo = do
  ringInfo :: [(Ring, Point)] <- for inhabitants $ \ring -> do
    let selector = ringClass ring & String.words & String.intercalate "." & ("." <>)
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
      , ringCenters = ringInfo & Map.fromList
      }

-}
