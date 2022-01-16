module Gemini.Html
  ( geminiHtmlView
  ) where

import           Relude

import           Data.Cyclic
import           Data.Finitary
import           Data.Permutation
import qualified Data.Sequence               as Seq
import           Data.Set.Optics
import qualified Data.Text                   as Text
import           Language.Javascript.JSaddle (JSVal, fromJSValUnchecked, jsg, toJSVal, (!), (#))
import           Optics                      hiding ((#))
import           Optics.State.Operators
import           Shpadoinkle                 hiding (text)
import qualified Shpadoinkle.Continuation    as Continuation
import qualified Shpadoinkle.Html            as Html

import           Gemini.Types
import           Gemini.UI.Actions
import           Utils


endDrag :: Store -> Store
endDrag store =
  flip execState store $ do
    drag <- use #drag
    case drag of
      Nothing -> pure ()
      Just drag -> do
        (#drag .= Nothing)
        let ring = drag ^. #ring
        let theta = drag ^. #currentAngle
        let n = floor $ (theta / 20) + 0.5
        let direction = if signum n > 0 then Clockwise else AntiClockwise
        let motion = Motion { amount = abs n, rotation = Rotation { ring, direction } }
        modify $ applyMotionToStore motion


ringCenter :: Ring -> Point
ringCenter = \case
  LeftRing   -> Point 150 150
  CenterRing -> Point (450 - 135) 150
  RightRing  -> Point (750 - 270) 150


-- dimensions
ringR = 150.0
diskR = (ringR / 7.0)
ringD = 2 * ringR
diskD = 2 * diskR

-- math utils
sine = sin . toRadians
cosine = cos . toRadians

toRadians :: Double -> Double
toRadians th = (th * pi) / 180.0

toDegrees :: Double -> Double
toDegrees th = (th * 180) / pi


draggedOver :: Store -> Set Location
draggedOver store = setOf (folded % to sibling % _Just) $ activeLocations store

activeLocations :: Store -> [Location]
activeLocations store = activeRing store & concatMap (\ring -> map (Location ring) inhabitants)

activeRing :: Store -> Maybe Ring
activeRing store = inhabitants & filter (isActive store) & preview (ix 0)

isActive :: Store -> Ring -> Bool
isActive store r = fromMaybe False $ do
  drag <- store ^. #drag
  pure $ drag ^. #ring == r

onDrag :: RawNode -> RawEvent -> JSM (Continuation m Store)
onDrag = \node event -> do
  geminiOffset <- toJSVal node # ("getBoundingClientRect" :: Text) $ ()
  g_x <- geminiOffset ! ("left" :: Text) >>= fromJSValUnchecked
  g_y <- geminiOffset ! ("top" :: Text) >>= fromJSValUnchecked

  client_x <- toJSVal event ! ("clientX" :: Text) >>= fromJSValUnchecked
  client_y <- toJSVal event ! ("clientY" :: Text) >>= fromJSValUnchecked

  let mouse = Point (client_x - g_x) (client_y - g_y)

  pure $ Continuation.Pure $
    (#drag % _Just) %~ \drag -> do
      let origin = drag ^. #ring % to ringCenter
      let Point x y = mouse ~~ origin
      let mouseAngle = atan2 y x & toDegrees
      drag & #currentAngle .~ (mouseAngle - initialAngle drag)



geminiHtmlView :: forall m. Store -> Html m Store
geminiHtmlView store =
  Html.div
    [ Html.class'
      [ ("gemini" :: Text, True)
      , ("dragging", isn't (#drag % _Nothing) store)
      ]
    , Html.onMouseup $ endDrag
    , Html.onMouseleave $ endDrag
    , ("mousemove", listenerProp onDrag)
    ]
    (  map ring inhabitants
    )
  where
    gemini = store ^. #gemini
    options = store ^. #options

    activeCycleMap :: Map Location Int
    activeCycleMap = store
      & itoListOf (#hover % #activeCycle % non (Cycle Empty) % ifolded)
      & map (\(i, x) -> (x, i + 1))
      & fromList
    ring :: Ring -> Html m Store
    ring r =
      Html.div
        [ Html.class'
          [ ("ring", True)
          , ("ring-" <> prettyCompactText r, True)
          , ("dragging", isJust $ dragAngle r)
          ]

        , Html.styleProp
            ( ("width", show ringD <> "px")
            : ("height", show ringD <> "px")
            : case dragAngle r of
                  Just angle -> [("transform", "rotate(" <> show angle <> "deg)")]
                  Nothing    -> []
            )
        ]
        ( disks r )

    dragAngle :: Ring -> Maybe Double
    dragAngle r = do
      drag <- store ^. #drag
      guard $ drag ^. #ring == r
      pure $ drag ^. #currentAngle

    disks :: Ring -> [Html m Store]
    disks ring = flip map inhabitants $ \position ->
      let
        location = Location ring position
        (color, diskLabel) =
          case gemini ^? geminiIx location of
            Just Disk { color, label } -> (Text.toLower $ show color, show label)
            Nothing                    -> ("unknown", "")
        angle = fromIntegral (unCyclic position) * 20.0 - 90.0
        r = ringR - diskR
        (x, y) =
          ( r * (1 + cosine angle)
          , r * (1 + sine angle)
          )

        defaultLabel :: Maybe Text
        defaultLabel = (options ^. #showLabels && store ^. #hover % #overMove % to not) `orNothing` diskLabel

        cycleLabel :: Maybe Text
        cycleLabel = activeCycleMap ^? ix location <&> show

        toLabelSpan label = Html.span [ ("className", "disk-label") ] [ Html.text label ]
      in
          Html.div
            ( Html.class'
              [ ("disk", True)
              , (color, True)
              , ("drag-disabled", isIntersection location)
              , ("hidden", draggedOver store ^. contains location)
              ]
            : Html.styleProp
              [ ("width", show diskD <> "px")
              , ("height", show diskD <> "px")
              , ("line-height", show diskD <> "px")
              , ("left", show x <> "px")
              , ("top", show y <> "px")
              ]
            : if isIntersection location
              then []
              else [ Html.onMousedown $ #drag ?~ DragState { ring, initialAngle = angle, currentAngle = 0 } ]
            )
            [ foldMap First [cycleLabel, defaultLabel] & getFirst & fromMaybe "" & toLabelSpan ]
