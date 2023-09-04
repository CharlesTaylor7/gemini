module Utils where

import Prelude
import Effect
import Debug (class DebugWarning)

logAnything :: DebugWarning => forall a. String -> a -> Effect Unit
logAnything = logAnythingF

foreign import logAnythingF :: forall a. String -> a -> Effect Unit

-- Technically a side effect, but this value is static for the whole run of the application
foreign import isTouchDevice :: Boolean
