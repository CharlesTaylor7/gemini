module Data.Array.ST.Extra
  ( unsafeNewSized
  , write
  ) where

import Prelude

import Control.Monad.ST (ST)
import Data.Array.ST (STArray, modify)

-- | This is marked unsafe because it creates an array with gaps.
-- | It's the responsibility of the caller to assign values into the gaps or break the guarantees of the Purescript Array type.
-- | Namely the purescript array API prevents arrays with gaps normally.
-- | They break expectations because mapping or iterating only visits locations with values, but the length will match the created value.
foreign import unsafeNewSized ::
  forall a region. Int -> ST region (STArray region a)

write :: forall a region. Int -> a -> STArray region a -> ST region Unit
write index val = void <<< modify index (const val)
