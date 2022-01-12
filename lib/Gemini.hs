{-# options_ghc -Wwarn #-}
module Gemini where

import           Relude                    hiding (Option)

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



-- | Store definition
data Store = Store
  { gemini  :: Gemini
  , history :: [Rotation]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Pretty Store where
  pretty Store { gemini } = show gemini


initialState :: Store
initialState = Store
  { gemini = solvedGemini
  , history = []
  }


-- | Components
rootView :: MonadIO m => Store -> Html m Store
rootView state =
  Html.div
    [ Html.className "gemini-app"
    , Html.tabIndex 0
    , Html.onKeydownM $ \key -> do
      traceShowM key
      let rotation = case key of
            -- the keyboard shortcuts are based on the top row of keys in the rightmost positions:
            -- T, Y, U, I, O, P
            Key.T -> Just $ Rotation LeftRing Clockwise
            Key.Y -> Just $ Rotation LeftRing AntiClockwise
            Key.U -> Just $ Rotation CenterRing Clockwise
            Key.I -> Just $ Rotation CenterRing AntiClockwise
            Key.O -> Just $ Rotation RightRing Clockwise
            Key.P -> Just $ Rotation RightRing AntiClockwise
            _     -> Nothing

      pure $ case rotation of
        Just r -> over #gemini (rotate r) . over #history (r :)
        _      -> identity
    ]
    [ geminiSvgView (state ^. #gemini)
    , debugView state
    ]

debugView :: Store -> Html m a
debugView state =
  Html.div
    [ Html.className "gemini-debug"]
    [ Html.text $ prettyCompactText (state ^. #history)
    ]


geminiSvgView :: Gemini -> Html m a
geminiSvgView gemini =
  Html.div
    [ Html.className "svg-wrapper"]
    [ Svg.bake $
      Svg.h "svg"
        [ ("class", "gemini-svg")
        , ("viewBox", "0 0 300 100")
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
      LeftRing   -> 100
      CenterRing -> 100 + ringOffset
      RightRing  -> 100 + ringOffset * 2
    ringR = 42
    diskR = 6
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



-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty


generalizeOptic :: (Functor m, Continuous f, Is k A_Lens) => Optic' k is s a -> (f m a -> f m s)
generalizeOptic arg = generalize $ toLensVL arg
