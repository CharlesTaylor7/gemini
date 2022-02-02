module Gemini.Utils
  ( prettyCompactText
  , zoomComponent
  , generalize
  , orNothing
  , loop
  , break
  , knownInt
  , natsUnder
  ) where

import           Relude                    hiding (break)

import           Optics
import           Prettyprinter             (Pretty (..))
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty
import           Shpadoinkle
import qualified Shpadoinkle.Lens


-- | Utilities
prettyCompactText :: forall a. Pretty a => a -> Text
prettyCompactText = Pretty.renderStrict . Pretty.layoutCompact . pretty


-- |
zoomComponent :: Functor m => Lens' s a -> s -> (a -> Html m a) -> Html m s
zoomComponent optic props component = component (props ^. optic) & generalize optic

generalize :: (Functor m, Continuous f, Is k A_Lens) => Optic' k ix s a -> (f m a -> f m s)
generalize optic = Shpadoinkle.Lens.generalize $ toLensVL optic


orNothing :: Alternative m => Bool -> a -> m a
bool `orNothing` a = if bool then pure a else empty


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
