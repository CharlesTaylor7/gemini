module Utils
  ( prettyCompactText
  , zoomComponent
  , generalize
  , orNothing
  ) where

import           Relude

import           Optics
import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty
import           Shpadoinkle
import qualified Shpadoinkle.Html          as Html
import qualified Shpadoinkle.Keyboard      as Key
import qualified Shpadoinkle.Lens


-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty


-- |
zoomComponent :: Functor m => Lens' s a -> s -> (a -> Html m a) -> Html m s
zoomComponent optic props component = component (props ^. optic) & generalize optic

generalize :: (Functor m, Continuous f) => Lens' s a -> (f m a -> f m s)
generalize optic = Shpadoinkle.Lens.generalize $ toLensVL optic


orNothing :: Bool -> a -> Maybe a
bool `orNothing` a = if bool then Just a else Nothing
