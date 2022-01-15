module Gemini.Html where

import           Relude

import           Data.Finitary
import qualified Data.Sequence    as Seq
import qualified Data.Text        as Text
import           Optics           hiding ((#))


import           Permutation
import           Shpadoinkle
import qualified Shpadoinkle.Html as Html

import           Gemini.Types
import           Utils


geminiHtmlView :: forall a m. Store -> Html m a
geminiHtmlView state =
  Html.div
    [ Html.className "gemini"
    ]
    (  map ring inhabitants
    )
  where
    gemini = state ^. #gemini
    options = state ^. #options

    activeCycleMap :: Map Location Int
    activeCycleMap = state
      & itoListOf (#hoveredCycle % non (Cycle Empty) % ifolded)
      & map (\(i, x) -> (x, i + 1))
      & fromList

    -- dimensions
    ringR = 150.0
    diskR = (ringR / 7.0)
    ringD = 2 * ringR
    diskD = 2 * diskR

    ring :: Ring -> Html m a
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

    disks :: Ring -> [Html m a]
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
          (options ^. #showLabels && isn't (#hoveredCycle % _Just) state) `orNothing` diskLabel

        cycleLabel :: Maybe Text
        cycleLabel = activeCycleMap ^? ix location <&> show

        toLabelSpan label = Html.span [ ("className", "disk-label") ] [ Html.text label ]

      in
        if not $ isCanonical location
        then Nothing
        else Just $
          Html.div
            [ Html.class' ["disk", color]
            , Html.styleProp
              [ ("width", show diskD <> "px")
              , ("height", show diskD <> "px")
              , ("line-height", show diskD <> "px")
              , ("left", show x <> "px")
              , ("top", show y <> "px")
              ]
            ]
            ( ((<>) `on` First) cycleLabel defaultLabel & toList <&> toLabelSpan )
