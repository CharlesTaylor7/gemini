module Gemini.Html where

import           Relude

import           Data.Cyclic
import           Data.Finitary
import           Data.Permutation
import qualified Data.Sequence               as Seq
import qualified Data.Text                   as Text
import           Language.Javascript.JSaddle (JSVal, fromJSValUnchecked, jsg, toJSVal, (!), (#))
import           Optics                      hiding ((#))
import           Shpadoinkle                 hiding (text)
import qualified Shpadoinkle.Continuation    as Continuation
import qualified Shpadoinkle.Html            as Html

import           Gemini.Types
import           Utils


endDrag :: Store -> Store
endDrag = #drag .~ Nothing


ringCenter :: Ring -> Point
ringCenter = \case
  LeftRing   -> Point 150 150
  CenterRing -> Point (300 - 135) 150
  RightRing  -> Point (450 - 270) 150


diskCenter :: Location -> Point
diskCenter location = Point x y <> ringCenter (location ^. #ring)
  where
    angle = fromIntegral (location ^. #position % #unCyclic) * 20.0 - 90.0
    r = ringR - diskR
    (x, y) =
      ( r * cosine angle
      , r * sine angle
      )


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


-- | angle between two points, in degrees
angle :: Point -> Point -> Point -> Double
angle center start end = acos (dot / mag)
  where
    dot = x1 * x2 + y1 * y2
    mag = sqrt $ (x1*x1 + y1*y1) * (x2 * x2 + y2 * y2)
    Point { x = x1, y = y1 } = start ~~ center
    Point { x = x2, y = y2 } = end ~~ center


geminiHtmlView :: forall m. Store -> Html m Store
geminiHtmlView state =
  Html.div
    [ Html.class'
      [ ("gemini" :: Text, True)
      , ("dragging", isn't (#drag % _Nothing) state)
      ]
    , Html.onMouseup $ endDrag
    , Html.onMouseleave $ endDrag
    , Html.listenRaw "mousemove" $ \node event -> do
        x <- toJSVal event ! ("offsetX" :: Text) >>= fromJSValUnchecked
        y <- toJSVal event ! ("offsetY" :: Text) >>= fromJSValUnchecked
        let z = x + y :: Double
        pure $ Continuation.Pure $ identity
    ]
    (  map ring inhabitants
    <> if not (state ^. #options % #debug)
       then []
       else
        flip map inhabitants $ \location ->
          let
            Point x y = diskCenter location
          in
            Html.div'
              [ Html.styleProp
                [ ("position", "absolute")
                , ("border-radius", "100%")
                , ("left",  show x <> "px")
                , ("top",  show y <> "px")
                , ("width", "10px")
                , ("height", "10px")
                , ("background-color", "pink")
                , ("z-index", "2")
                ]
              , Html.textProp "data-location" (show location)
              ]
    )
  where
    gemini = state ^. #gemini
    options = state ^. #options

    activeCycleMap :: Map Location Int
    activeCycleMap = state
      & itoListOf (#hover % #activeCycle % non (Cycle Empty) % ifolded)
      & map (\(i, x) -> (x, i + 1))
      & fromList


    ring :: Ring -> Html m Store
    ring r =
      Html.div
        [ Html.class' [ "ring", "ring-" <> prettyCompactText r ]
        , Html.styleProp
            [ ("width", show ringD <> "px")
            , ("height", show ringD <> "px")
            ]
        ]
        ( disks r )


    disks :: Ring -> [Html m Store]
    disks ring = catMaybes $ flip map inhabitants $ \position ->
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
        defaultLabel =
          (options ^. #showLabels && state ^. #hover % #overMove % to not) `orNothing` diskLabel

        cycleLabel :: Maybe Text
        cycleLabel = activeCycleMap ^? ix location <&> show

        toLabelSpan label = Html.span [ ("className", "disk-label") ] [ Html.text label ]
      in
        isCanonical location `orNothing`
          Html.div
            [ Html.class'
                [ ("disk", True)
                , (color, True)
                , ("dragging", Just location == (state ^? #drag % _Just % #start))
                ]
            , Html.onMousedown $ #drag ?~ DragState { start = location, angle = 0, ring }
            , Html.styleProp
              [ ("width", show diskD <> "px")
              , ("height", show diskD <> "px")
              , ("line-height", show diskD <> "px")
              , ("left", show x <> "px")
              , ("top", show y <> "px")
              ]
            ]
            ( ((<>) `on` First) cycleLabel defaultLabel & toList <&> toLabelSpan )

