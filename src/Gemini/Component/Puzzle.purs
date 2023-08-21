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


component :: Nut
component = Deku.do
  D.div
    [ ]
    [ text_ "Hello" ]

{-
  
geminiView :: Nut 
geminiView =
  D.div
    [ D.class'
      [ ("gemini" :: String, True)
      , ("dragging", isn't (#drag % _Nothing) store)
      ]
    ] $
    concat
      [ [D.div' [D.className "background"]]
      , map ringView inhabitants
      ]
  where
    gemini = store ^. #gemini
    options = store ^. #options
    mobile = options ^. #mobile
    dragged = dragAngle store

    highlighted :: Set Location
    highlighted =
      if options ^. #highlightPairs
      then setOf (to solutionPairs % folded % each) gemini
      else mempty

    activeCycle :: Set Location
    activeCycle = setOf (#hover % _Just % #cycle % _Just % folded) store

    hidden :: Set Location
    hidden = hiddenLocations store

    activeMoveLabels :: Map Location String
    activeMoveLabels = store
      & itoListOf (#hover % _Just % #move % #moveCycles % each <%> ifolded)
      & map (\((i, j), x) -> (x, show (i + 1) <> String.singleton (toLetter j)))
      & fromList

    ringView :: Ring -> Nut
    ringView ring =
      D.div [ D.className $ ringClass ring] $
        disks ring

    disks :: Ring -> [Nut]
    disks ring = flip map inhabitants $ \position ->
      let
        location = Location ring position
        (color, diskLabel) =
          case gemini ^? geminiIx location of
            Just Disk { color, label } -> (String.toLower $ show color, show label)
            Nothing                    -> ("unknown", "")

        draggedAngle :: Angle
        draggedAngle = fromMaybe mempty $ do
          (draggedRing, angle) <- dragged
          guard $ ring == draggedRing ^. #ring
          pure angle

        animatedAngle :: Angle
        animatedAngle = fromMaybe mempty $ do
          frame <- store ^. #animation % #frame
          let rotation = frame ^. #motion ^. #rotation
          let sign = if rotation ^. #direction == Clockwise then 1 else -1
          guard $ ring == rotation ^. #ring
          pure $ Turns $
            let
              n = sign * fromIntegral (frame ^. #tick)
              d = store ^. #animation % #ticksPerRotation % to (*18) % to fromIntegral
            in
              n / d

        diskAngle = angleOnCircle position <> draggedAngle <> animatedAngle

        k = 43
        (x, y) =
          ( k * (1 + cosine diskAngle)
          , k * (1 + sine diskAngle)
          )

        defaultLabel :: Maybe String
        defaultLabel = (options ^. #showLabels && isn't (#hover % _Just) store) `orNothing` diskLabel

        cycleLabel :: Maybe String
        cycleLabel = activeMoveLabels ^? ix location

        toLabelSpan label = D.span [ ("className", "disk-label") ] [ D.text label ]

        isDraggedDisk :: Location -> Bool
        isDraggedDisk l = Just l == dragged ^? _Just % _1
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
            if mobile
            then []
            else [ foldMap First [cycleLabel, defaultLabel] & getFirst & fromMaybe "" & toLabelSpan ]



hiddenLocations :: Store -> Set Location
hiddenLocations store =
  ambiguousLocations
  & map (\(left, right) -> if Just (right ^. #ring) == activeRing store then left else right)
  & fromList
  where
    activeRing :: Store -> Maybe Ring
    activeRing s =
      s ^? to dragAngle % #_Just % _1 % #ring <|>
      s ^? #animation % #frame % #_Just % #motion % #rotation % #ring


angleOnCircle :: forall n. NonZero n => Cyclic n -> Angle
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


toLetter :: Int -> Char
toLetter i = toEnum $ i + 97

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
