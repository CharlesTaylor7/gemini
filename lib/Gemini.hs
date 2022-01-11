{-# options_ghc -Wwarn #-}
module Gemini where

import           Relude                      hiding (Option)

import           Optics                      hiding ((#))
import           Prettyprinter               (Pretty (..))
import qualified Prettyprinter               as Pretty
import qualified Prettyprinter.Render.Text   as Pretty



import           JSDOM                       (currentDocumentUnchecked)
import           Language.Javascript.JSaddle (JSVal, (#))

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
    , debugView state
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
    [ bakeSvg $
      hSvg "svg"
        [ ("class", "gemini-svg")
        , ("viewBox", "0 0 100 100")
        ]
        [ hSvg "circle"
          [ ("r", "50")
          , ("cx", "50")
          , ("cy", "50")
          ]
          []
        ]
    ]



-- | Svg elements
-- https://stackoverflow.com/a/3642265
bakeSvg :: SvgElement -> Html m a
bakeSvg svgElement = baked $ do
  document' <- currentDocumentUnchecked

  let svgNamespace :: Text
      svgNamespace = "http://www.w3.org/2000/svg"

  let render :: SvgElement -> JSM JSVal
      render SvgElement { name, attributes, children } = do
        element' <- document' # ("createElementNS" :: Text) $ (svgNamespace, name)
        for_ attributes $ element' # ("setAttribute" :: Text)
        for_ children $ \child -> do
          child' <- render child
          element' # ("appendChild" :: Text) $ child'
        pure element'

  el' <- render svgElement
  pure (RawNode el', pure Continuation.done)



hSvg = SvgElement
data SvgElement = SvgElement
  { name       :: !Text
  , attributes :: [(Text, Text)]
  , children   :: [SvgElement]
  }



-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty
