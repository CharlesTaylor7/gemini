{-# options_ghc -Wwarn #-}
module Gemini where

import           Relude                      hiding (Option)

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


-- | Store transformations
-- The 6 basic motions are called:
-- L, L', C, C', R, R'
-- L is a clockwise rotation of the leftmost ring, L' is anticlockwise
-- C is for the central ring
-- R is for the rightmost ring
rotateL, rotateL', rotateC, rotateC', rotateR, rotateR' :: Store -> Store
rotateL = identity
rotateL' = identity
rotateC = identity
rotateC' = identity
rotateR = identity
rotateR' = identity
-- the keyboard shortcuts are based on the top row of keys in the rightmost positions:
-- T = L, Y = L', U = C, I = C', O = R, P = R'

-- | Components
rootView :: MonadIO m => Store -> Html m Store
rootView state =
  Html.div
    [ Html.className "gemini-app"
    , Html.tabIndex 0
    , Html.onKeydownC \case
      Key.T -> Pure rotateL
      Key.Y -> Pure rotateL'
      Key.U -> Pure rotateC
      Key.I -> Pure rotateC'
      Key.O -> Pure rotateR
      Key.P -> Pure rotateR'
      _     -> done
    ]
    [ geminiSvgView (state ^. #gemini)
    -- , debugView state
    ]

debugView :: Store -> Html m Store
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
        ( map ringDisks [LeftRing, CenterRing, RightRing] )
    ]


ringDisks :: Ring -> SvgElement
ringDisks r = Svg.h "g" [] (ringOutline : disks)
  where
    -- law of sines
    ringOffset = (ringR - diskR) * (sine 100.0 / sine 40.0)
    -- big angle = 100 degrees
    -- small angle = 40 degrees
    -- ringOffset = radius * (sin 100 / sin 40)
    sine = sin . toRadians
    toRadians :: Double -> Double
    toRadians th = (th * pi) / 180.0

    ringX = case r of
      LeftRing   -> 100
      CenterRing -> 100 + ringOffset
      RightRing  -> 100 + ringOffset * 2
    ringR = 42
    diskR = 6
    ringY = ringR

    ringOutline :: SvgElement
    ringOutline =
      Svg.circle
        [ ("r", show ringR)
        , ("cx", show ringX)
        , ("cy", show ringY)
        , ("fill", "none")
        ]

    diskProps :: Svg.AttributeList
    diskProps =
      [ ("r", show diskR)
      , ("cx", show ringX)
      , ("cy", show diskR)
      ]

    disks :: [SvgElement]
    disks = flip map [0..17] $ \i ->
      Svg.circle
        ( ("transform", "rotate(" <> show (i * 20) <> "," <> show ringX <> "," <> show ringY <> ")")
        : diskProps
        )


-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty
