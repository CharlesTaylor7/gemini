{-# options_ghc -Wwarn #-}
module Svg
  ( h , bake , SvgElement, AttributeList
  , circle
  ) where

import           Relude                      hiding (Option)

import           Optics                      hiding ((#))
import           Prettyprinter               (Pretty (..))
import qualified Prettyprinter               as Pretty
import qualified Prettyprinter.Render.Text   as Pretty



import           JSDOM                       (currentDocumentUnchecked)
import           Language.Javascript.JSaddle (JSVal, (#))

import           Shpadoinkle                 (Html, JSM, RawNode (..), baked)
import qualified Shpadoinkle.Continuation    as Continuation
import qualified Shpadoinkle.Html            as Html
import qualified Shpadoinkle.Keyboard        as Key


type AttributeList = [(Text, Text)]


circle :: AttributeList -> SvgElement
circle attributes = SvgElement
  { name = "circle"
  , children = []
  , attributes
  }
-- | Svg elements
-- https://stackoverflow.com/a/3642265
bake :: SvgElement -> Html m a
bake svgElement = baked $ do
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


h = SvgElement
data SvgElement = SvgElement
  { name       :: !Text
  , attributes :: AttributeList
  , children   :: [SvgElement]
  }
