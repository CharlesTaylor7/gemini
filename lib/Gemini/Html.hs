module Gemini.Html where

import           Relude

import qualified Data.Text                 as Text
import           Optics                    hiding ((#))
import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty


import           Shpadoinkle
import qualified Shpadoinkle.Html          as Html
import qualified Shpadoinkle.Keyboard      as Key
import           Shpadoinkle.Lens          (generalize)

import           Gemini.Types



geminiHtmlView :: forall a m. Gemini -> Html m a
geminiHtmlView gemini =
  Html.div
    [ Html.className "gemini-wrapper"
    ]
    [ Html.div
        [ Html.className "gemini"
        ]
        (  map ring [LeftRing, CenterRing, RightRing]
        )
    ]
  where
    -- dimensions
    ringR = 100.0
    diskR = (ringR / 7.0)
    ringY = ringR
    ringX r = case r of
      LeftRing   -> ringR
      CenterRing -> ringR + ringOffset
      RightRing  -> ringR + ringOffset * 2
    ringD = 2 * ringR
    diskD = 2 * diskR

    ring :: Ring -> Html m a
    ring r =
      Html.div
        [ Html.className "ring"
        , Html.styleProp
            [ ("width", show ringD <> "px")
            , ("height", show ringD <> "px")
            , ("margin-right", show (-ringOffset) <> "px")
            ]
        ]
        ( disks r )


    -- law of sines
    ringOffset = (ringR - diskR) * (sine 80.0 / sine 50.0)
    -- big angle = 100 degrees
    -- small angle = 40 degrees
    -- ringOffset = radius * (sin 100 / sin 40)
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
          ( r * (cosine angle) + ringX ring
          , r * (sine angle) + ringY
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
          [ Html.span
              [ ("className", "disk-label")
              ]
              [ Html.text label
              ]
          ]
