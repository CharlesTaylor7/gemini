module Gemini.Svg where

import           Relude

import qualified Data.Text                 as Text
import           Optics                    hiding ((#))
import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty

import           Svg                       (SvgElement)
import qualified Svg

import           Shpadoinkle
import qualified Shpadoinkle.Html          as Html
import qualified Shpadoinkle.Keyboard      as Key
import           Shpadoinkle.Lens          (generalize)

import           Gemini.Types



geminiSvgView :: Gemini -> Html m a
geminiSvgView gemini =
  Html.div
    [ Html.className "gemini-wrapper"]
    [ Svg.bake $
      Svg.h "svg"
        [ ("class", "gemini")
        , ("viewBox", "0 0 210 100")
        ]
        (  map ringOutline [LeftRing, CenterRing, RightRing]
        <> map ringDisks [LeftRing, CenterRing, RightRing]
        )
    ]
  where
    ringOutline :: Ring -> SvgElement
    ringOutline r =
      Svg.circle
        [ ("class", "ring")
        , ("r", show ringR)
        , ("cx", show $ ringX r)
        , ("cy", show ringY)
        ]

    ringDisks :: Ring -> SvgElement
    ringDisks r = Svg.h "g" [] $ disks r

    -- law of sines
    ringOffset = (ringR - diskR) * (sine 80.0 / sine 50.0)
    -- big angle = 100 degrees
    -- small angle = 40 degrees
    -- ringOffset = radius * (sin 100 / sin 40)
    sine = sin . toRadians
    cosine = cos . toRadians
    toRadians :: Double -> Double
    toRadians th = (th * pi) / 180.0

    ringX r = case r of
      LeftRing   -> ringR
      CenterRing -> ringR + ringOffset
      RightRing  -> ringR + ringOffset * 2
    ringR = 50.0
    diskR = (ringR / 7.0)
    ringY = ringR

    disks :: Ring -> [SvgElement]
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
        Svg.h "g"
          [ ("transform", "translate(" <> show x <> "," <> show y <> ")")
          , ("class", "disk-" <> color)
          ]
          [ Svg.circle [("r", show diskR)]
          , Svg.text
              [ ("class", "disk-label")
              , ("dx", "-2.5")
              , ("dy", "3")
              ]
              label
          ]


