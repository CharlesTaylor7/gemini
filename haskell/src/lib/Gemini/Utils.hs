module Gemini.Utils
  ( prettyCompactText
  , prettyText
  , zoomComponent
  , generalize
  , orNothing
  , orEmpty
  , htmlWhen
  , loop
  , break
  , knownInt
  , natsUnder
  , IsLens
  , is
  ) where

import           Relude                    hiding (break)

import           Optics
import           Optics.Core.Extras        (is)
import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty
import           Shpadoinkle
import qualified Shpadoinkle.Continuation
import qualified Shpadoinkle.Html          as Html


-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty

prettyText :: forall a. Pretty a => a -> Text
prettyText = Pretty.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

type IsLens k = (Is k A_Getter, Is k A_Setter)
-- |
zoomComponent
  :: Functor m
  => (IsLens k)
  => Optic' k ix s a
  -> s -> (a -> Html m a) -> Html m s
zoomComponent optic props component = component (props ^. optic) & generalize optic

generalize
  :: forall m f k ix s a
  . (Functor m, Continuous f, IsLens k)
  => Optic' k ix s a
  -> (f m a -> f m s)
generalize optic = Shpadoinkle.Continuation.liftC (set optic) (view optic)
  where
--flip set optic) (view optic)


orNothing :: Bool -> a -> Maybe a
bool `orNothing` a = if bool then pure a else Nothing

orEmpty :: Bool -> Html m a -> Html m a
bool `orEmpty` a = if bool then a else Html.div'_

htmlWhen :: forall m a. Bool -> Html m a -> Html m a
htmlWhen = orEmpty


-- looping utilities
loop :: Monad m => MaybeT m a -> m ()
loop = void . runMaybeT . forever

break :: Monad m => MaybeT m a
break = MaybeT $ pure $ Nothing


-- * make KnownNat easier to use
knownInt :: forall n. KnownNat n => Int
knownInt = fromIntegral $ natVal @n (Proxy :: Proxy n)

natsUnder :: forall bound. KnownNat bound => [Int]
natsUnder = [0..knownInt @bound - 1]
