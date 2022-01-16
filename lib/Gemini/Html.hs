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
  CenterRing -> Point (450 - 135) 150
  RightRing  -> Point (750 - 270) 150


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

toDegrees :: Double -> Double
toDegrees th = (th * 180) / pi

-- | angle between two points, in degrees
angleBetween :: Point -> Point -> Double
angleBetween start end = toDegrees $ sign * acos (dot / mag)
  where
    -- angle has the same sign as the cross product
    sign = if x1*y2 >= x2*y1 then 1 else -1
    dot = x1*x2 + y1*y2
    mag = sqrt $ (x1*x1 + y1*y1) * (x2*x2 + y2*y2)
    Point { x = x1, y = y1 } = start
    Point { x = x2, y = y2 } = end


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
    ]
    (  map ring inhabitants
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
      drag <- state ^. #drag
      guard $ drag ^. #ring == r
      pure $ drag ^. #currentAngle


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
            [ Html.class' [ "disk" , color ]
            , Html.onMousedown $ #drag ?~ DragState { ring, initialAngle = angle, currentAngle = 0 }
            , Html.styleProp
              [ ("width", show diskD <> "px")
              , ("height", show diskD <> "px")
              , ("line-height", show diskD <> "px")
              , ("left", show x <> "px")
              , ("top", show y <> "px")
              ]
            ]
            [ foldMap First [cycleLabel, defaultLabel] & getFirst & fromMaybe "" & toLabelSpan ]

