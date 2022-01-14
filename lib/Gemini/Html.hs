module Gemini.Html where

import           Relude

import qualified Data.Text        as Text
import           Optics           hiding ((#))


import           Shpadoinkle
import qualified Shpadoinkle.Html as Html

import           Gemini.Types



geminiHtmlView :: forall a m. Options -> Gemini -> Html m a
geminiHtmlView options gemini =
  Html.div
    [ Html.className "gemini"
    ]
    (  map ring [LeftRing, CenterRing, RightRing]
    )
  where
    -- dimensions
    ringR = 150.0
    diskR = (ringR / 7.0)
    ringD = 2 * ringR
    diskD = 2 * diskR
    ringShift = \case
      LeftRing   -> ("left", show (0.9 * ringR) <> "px")
      CenterRing -> ("left", "0")
      RightRing  -> ("right", show (0.9 * ringR) <> "px")

    ring :: Ring -> Html m a
    ring r =
      Html.div
        [ Html.className "ring"
        , Html.styleProp
            [ ("width", show ringD <> "px")
            , ("height", show ringD <> "px")
            , ringShift r
            ]
        ]
        ( disks r )


    sine = sin . toRadians
    cosine = cos . toRadians
    toRadians :: Double -> Double
    toRadians th = (th * pi) / 180.0

    disks :: Ring -> [Html m a]
    disks ring = flip map [0..17] $ \position ->
      let
        (color, label) =
          case gemini ^? geminiIx (Location ring position) of
            Just Disk { color, label } -> (Text.toLower $ show color, show label)
            Nothing                    -> ("unknown", "unknown")
        r = ringR - diskR
        angle = fromIntegral position * 20.0 - 90.0
        (x, y) =
          ( r * (1 + cosine angle)
          , r * (1 + sine angle)
          )
      in
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
          if options ^. #showLabels
          then [ Html.span [ ("className", "disk-label") ] [ Html.text label ] ]
          else []

