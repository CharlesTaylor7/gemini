{-# options_ghc -Wwarn #-}
module Svg
  ( h , bake , SvgElement, AttributeList
  , circle, text
  ) where

import           Relude                      hiding (Option)


import           JSDOM                       (currentDocumentUnchecked)
import           Language.Javascript.JSaddle (JSVal, (#))

import           Shpadoinkle                 (Html, JSM, RawNode (..), baked)
import qualified Shpadoinkle.Continuation    as Continuation


-- | Definitions
data SvgElement
  = SvgElement
  { name       :: !Text
  , attributes :: AttributeList
  , children   :: [SvgElement]
  }
  | SvgText
  { attributes :: AttributeList
  , contents   :: !Text
  }


type AttributeList = [(Text, Text)]


h :: Text -> AttributeList -> [SvgElement] -> SvgElement
h = SvgElement

circle :: AttributeList -> SvgElement
circle attributes = h "circle" attributes []

text :: AttributeList -> Text -> SvgElement
text = SvgText

-- | Svg elements
-- https://stackoverflow.com/a/3642265
bake :: SvgElement -> Html m a
bake svgElement = baked $ do
  document' <- currentDocumentUnchecked

  let svgNamespace :: Text
      svgNamespace = "http://www.w3.org/2000/svg"

  let render :: SvgElement -> JSM JSVal
      render SvgText { attributes, contents } = do
        element' <- document' # ("createElementNS" :: Text) $ (svgNamespace, "text" :: Text)
        for_ attributes $ element' # ("setAttribute" :: Text)
        void $ element' # ("append" :: Text) $ contents
        pure element'

      render SvgElement { name, attributes, children } = do
        element' <- document' # ("createElementNS" :: Text) $ (svgNamespace, name)
        for_ attributes $ element' # ("setAttribute" :: Text)
        for_ children $ \child -> do
          child' <- render child
          element' # ("appendChild" :: Text) $ child'
        pure element'

  el' <- render svgElement
  pure (RawNode el', pure Continuation.done)
