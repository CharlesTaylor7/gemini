module Gemini.Prelude
  ( module Prelude
  , module Debug
  , module Data.Nat
  , module Data.Angle
  , module Data.Cyclic
  , module Data.Finitary
  , module Data.Group
  , module Data.Gemini
  , module Data.Set
  , module Data.Map
  , module Data.Maybe
  , module Data.Point
  , module Data.Foldable
  , module Data.Tuple.Nested
  , module Effect
  , module Effect.Console
  , module Partial.Unsafe
  , module Deku.Attribute
  , module FRP.Event
  , module Gemini.Types
  , module Gemini.Store
  , logAnything
  ) where

import Prelude hiding (class Ring)
import Debug (class DebugWarning)
import Data.Nat
import Data.Angle (Angle, AngleUnit(..), arctan, as, cosine, sine, (:*))
import Data.Cyclic (Cyclic, CyclicOrdering(..), compareCyclic, cyclic, unCyclic)
import Data.Finitary (class Finitary, inhabitants)
import Data.Group (class Group, invert, pow)
import Data.Set (Set)
import Data.Map (Map)
import Data.Point (Point(..))
import Data.Maybe (Maybe(..))
import Data.Foldable (fold, foldMap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafeCrashWith)
import Deku.Attribute (Attribute, class Attr)
import FRP.Event (Event)
import Gemini.Store (Store)
import Data.Gemini
  ( Gemini
  , Rotation(..)
  , rotation
  , RotationDirection(..)
  , Motion(..)
  , Ring(..)
  , Location(..)
  , Choice(..)
  , Chosen(..)
  )
import Gemini.Types
  ( Hover(..)
  , Drag(..)
  , Confetti(..)
  , Stats(..)
  , Move(..)
  )

logAnything :: DebugWarning => forall a. String -> a -> Effect Unit
logAnything = logAnythingF

foreign import logAnythingF :: forall a. String -> a -> Effect Unit
