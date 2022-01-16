module Gemini.Html
  ( geminiHtmlView
  ) where

import           Relude

import           Data.Cyclic
import           Data.Finitary
import           Data.Permutation
import qualified Data.Sequence               as Seq
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
endDrag state =
  flip execState state $ do
    drag <- use #drag
    case drag of
      Nothing -> pure ()
      Just drag -> do
        (#drag .= Nothing)
        let ring = drag ^. #ring
        let theta = drag ^. #currentAngle
        let n = truncate $ (theta / 20) - 0.5
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
        defaultLabel = (options ^. #showLabels && state ^. #hover % #overMove % to not) `orNothing` diskLabel

        cycleLabel :: Maybe Text
        cycleLabel = activeCycleMap ^? ix location <&> show

        toLabelSpan label = Html.span [ ("className", "disk-label") ] [ Html.text label ]
      in
          Html.div
            [ Html.class'
              [ ("disk", True)
              , (color, True)
              , ("drag-disabled", isIntersection location)
              ]
            , Html.onMousedown $
                if isIntersection location
                then identity
                else (#drag ?~ DragState { ring, initialAngle = angle, currentAngle = 0 })
            , Html.styleProp
              [ ("width", show diskD <> "px")
              , ("height", show diskD <> "px")
              , ("line-height", show diskD <> "px")
              , ("left", show x <> "px")
              , ("top", show y <> "px")
              ]
            ]
            [ foldMap First [cycleLabel, defaultLabel] & getFirst & fromMaybe "" & toLabelSpan ]
