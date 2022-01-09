{-# options_ghc -Wwarn #-}
module Gemini where

import           Relude                    hiding (Option)

import           Control.Exception         (throwIO)
import           Data.List                 (findIndex)
import           Data.Text                 (pack, unpack)
import           Data.Vector               (Vector)
import           Optics
import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty

import           Shpadoinkle
import qualified Shpadoinkle.Continuation  as Continuation
import qualified Shpadoinkle.Html          as Html
import qualified Shpadoinkle.Keyboard      as Key

import           Gemini.Types



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


-- | state functions
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
    [ debugView state
    ]


debugView state =
  Html.div_
    [ Html.br'_
    , Html.br'_
    , Html.br'_
    , Html.text $ prettyCompactText state
    ]


-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty
