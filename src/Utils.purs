module Utils where

import Prelude
import Effect
import Debug (class DebugWarning)

logAnything :: DebugWarning => forall a. String -> a -> Effect Unit
logAnything = logAnythingF

foreign import logAnythingF :: forall a. String -> a -> Effect Unit
