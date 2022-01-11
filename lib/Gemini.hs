{-# options_ghc -Wwarn #-}
module Gemini where

import           Relude                      hiding (Option)

import qualified Data.Text                   as Text
import           Optics                      hiding ((#))
import           Prettyprinter               (Pretty (..))
import qualified Prettyprinter               as Pretty
import qualified Prettyprinter.Render.Text   as Pretty

import           JSDOM                       (currentDocumentUnchecked)
import           Language.Javascript.JSaddle (JSVal, (#))

import           Svg                         (SvgElement)
import qualified Svg

import           Shpadoinkle
import qualified Shpadoinkle.Continuation    as Continuation
import qualified Shpadoinkle.Html            as Html
import qualified Shpadoinkle.Keyboard        as Key
import           Shpadoinkle.Lens            (generalize)

import           Gemini.Types



-- | Store definition
data Store = Store
  { gemini :: Gemini
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Pretty Store where
  pretty Store { gemini } = ""


initialState :: Store
initialState = Store
  { gemini = solvedGemini
  }


-- | Components
rootView :: MonadIO m => Store -> Html m Store
rootView state = generalizeOptic #gemini $
  Html.div
    [ Html.className "gemini-app"
    , Html.tabIndex 0
    , Html.onKeydownC \case
      -- the keyboard shortcuts are based on the top row of keys in the rightmost positions:
      -- T, Y, U, I, O, P
      Key.T -> Pure $ rotate LeftRing Clockwise
      Key.Y -> Pure $ rotate LeftRing AntiClockwise
      Key.U -> Pure $ rotate CenterRing Clockwise
      Key.I -> Pure $ rotate CenterRing AntiClockwise
      Key.O -> Pure $ rotate RightRing Clockwise
      Key.P -> Pure $ rotate RightRing AntiClockwise
      _     -> done
    ]
    [ geminiSvgView (state ^. #gemini)
    , debugView state
    ]

debugView :: Store -> Html m a
debugView state =
  Html.div
    [ Html.className "gemini-debug"]
    [ Html.text $ show state
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
    toRadians :: Double -> Double
    toRadians th = (th * pi) / 180.0

    ringX r = case r of
      LeftRing   -> 100
      CenterRing -> 100 + ringOffset
      RightRing  -> 100 + ringOffset * 2
    ringR = 42
    diskR = 6
    ringY = ringR

    diskProps :: Ring -> Svg.AttributeList
    diskProps r =
      [ ("r", show diskR)
      , ("cx", show $ ringX r)
      , ("cy", show diskR)
      , ("stroke", "none")
      ]

    disks :: Ring -> [SvgElement]
    disks r = flip map [0..17] $ \i ->
      let
        (color, label) =
          case gemini ^? geminiIx (Location r i) of
            Just Disk { color, label } -> (Text.toLower $ show color, show label)
            Nothing                    -> ("unknown", "unknown")
      in
        Svg.h "g"
          []
          [ Svg.h "text" [] []
          , Svg.circle
              ( ("transform", "rotate(" <> show (i * 20) <> "," <> show (ringX r) <> "," <> show ringY <> ")")
              : ("class", "disk-" <> color)
              : diskProps r
              )
          ]



-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty


generalizeOptic :: (Functor m, Continuous f, Is k A_Lens) => Optic' k is s a -> (f m a -> f m s)
generalizeOptic arg = generalize $ toLensVL arg
