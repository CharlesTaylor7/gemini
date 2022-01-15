module Gemini.Html where

import           Relude

import           Data.Finitary
import qualified Data.Sequence               as Seq
import qualified Data.Text                   as Text
import           Language.Javascript.JSaddle (JSVal, fromJSValUnchecked, jsg, toJSVal, (!), (#))
import           Optics                      hiding ((#))
import           Shpadoinkle                 hiding (text)
import qualified Shpadoinkle.Continuation    as Continuation
import qualified Shpadoinkle.Html            as Html

import           Gemini.Types
import           Permutation
import           Utils

endDrag :: Store -> Store
endDrag = #drag .~ Nothing


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
    )
  where
    gemini = state ^. #gemini
    options = state ^. #options

    activeCycleMap :: Map Location Int
    activeCycleMap = state
      & itoListOf (#hover % #activeCycle % non (Cycle Empty) % ifolded)
      & map (\(i, x) -> (x, i + 1))
      & fromList

    -- dimensions
    ringR = 150.0
    diskR = (ringR / 7.0)
    ringD = 2 * ringR
    diskD = 2 * diskR

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


    sine = sin . toRadians
    cosine = cos . toRadians
    toRadians :: Double -> Double
    toRadians th = (th * pi) / 180.0

    disks :: Ring -> [Html m Store]
    disks ring = catMaybes $ flip map [0..17] $ \position ->
      let
        location = Location ring position
        (color, diskLabel) =
          case gemini ^? geminiIx location of
            Just Disk { color, label } -> (Text.toLower $ show color, show label)
            Nothing                    -> ("unknown", "")
        r = ringR - diskR
        angle = fromIntegral position * 20.0 - 90.0
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
