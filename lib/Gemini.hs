{-# options_ghc -Wwarn #-}
module Gemini where

import           Relude                      hiding (Option)

import           Control.Exception           (throwIO)
import           Data.List                   (findIndex)
import           Data.Text                   (pack, unpack)
import           Data.Vector                 (Vector)
import           Optics                      hiding ((#))
import           Prettyprinter               (Pretty (..))
import qualified Prettyprinter               as Pretty
import qualified Prettyprinter.Render.Text   as Pretty



import           JSDOM                       (currentDocumentUnchecked)
import           Language.Javascript.JSaddle (JSM, JSString, MonadJSM, Object, fromJSVal, toJSVal, (#))

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
  Html.div_
    [ Html.br'_
    , Html.br'_
    , Html.br'_
    , Html.text $ show state
    ]


geminiSvgView :: Gemini -> Html m a
geminiSvgView gemini =
  hSvg "svg"
    [ ("class", "gemini-svg")
    , ("viewBox", "0 0 100 100")
    ]

-- | Svg elements
hSvg :: Text -> [(Text, Text)] -> Html m a
hSvg name attributes = baked $ do
  document' <- currentDocumentUnchecked
  container' <- document' # ("createElement" :: Text) $ name
  for_ attributes $ container' # ("setAttribute" :: Text)
  return (RawNode container', pure done)



-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty
